// embed-questions: fills empty embeddings in public.question_embedding with gte-small.
//
// The database calls it (sql/8_search.sql, private.embed_questions()) every 10 seconds while any embedding
// is empty, with the token it keeps in Vault as embed_questions_token; anything else is turned away. An Edge
// Function gets about 2 seconds of CPU per request, so each call embeds only a few questions, one at a time,
// and saves each as soon as it is made: a call cut off part way keeps what it did.
// A question is locked while it is embedded, so calls at once never embed the same one. A question that
// can't be embedded is skipped for the rest of the call, so it never holds up the others. Running it again
// does nothing more: it only fills empty ones.
import 'jsr:@supabase/functions-js/edge-runtime.d.ts'
import postgres from 'npm:postgres@3.4.5'

const sql = postgres(Deno.env.get('SUPABASE_DB_URL')!, { prepare: false })
const model = new Supabase.ai.Session('gte-small')
const perCall = 4 // about all the CPU allows, with the model to load in a new worker

Deno.serve(async (req) => {
  if (req.method !== 'POST') return Response.json({ message: 'Only POST.' }, { status: 405 })
  const [secret] = await sql`
    select decrypted_secret as token from vault.decrypted_secrets where name = 'embed_questions_token'`
  if (!secret?.token || req.headers.get('x-embed-token') !== secret.token) {
    return Response.json({ message: 'Not allowed.' }, { status: 401 })
  }

  let embedded = 0
  const failed: string[] = [] // component_ids, as postgres gives a bigint
  while (embedded + failed.length < perCall) {
    let current: string | undefined
    try {
      const done = await sql.begin(async (tx) => {
        const [row] = await tx`
          select component_id, content from public.question_embedding
          where embedding is null and not (component_id = any(${failed}::bigint[]))
          order by component_id
          limit 1
          for update skip locked`
        if (!row) return false
        current = row.component_id
        const started = performance.now()
        const embedding = await model.run(row.content, { mean_pool: true, normalize: true })
        console.log(`embedded ${row.component_id} (${row.content.length} characters) in ${Math.round(performance.now() - started)} ms`)
        await tx`
          update public.question_embedding set embedding = ${JSON.stringify(embedding)}, embedded_at = now()
          where component_id = ${row.component_id}`
        return true
      })
      if (!done) break
      embedded += 1
    } catch (problem) {
      if (current === undefined) throw problem // the database, not the question: nothing to skip
      console.error(`couldn't embed ${current}: ${problem instanceof Error ? problem.message : problem}`)
      failed.push(current)
    }
  }
  return Response.json({ embedded, failed })
})
