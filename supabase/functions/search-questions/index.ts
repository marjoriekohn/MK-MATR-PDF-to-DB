// search-questions: the questions closest in meaning to a search, for the dashboard's Questions Explorer.
//
//   POST { "query": "trouble sleeping", "threshold": 0.85, "count": 50 }
//   -> [{ "component_id": 123, "similarity": 0.86 }, ...], closest first
//
// It embeds the search with gte-small, as embed-questions embedded the questions, and asks the database's
// match_questions() with the caller's own key, so the database decides what the key may read. Edge Functions
// don't check publishable keys (sb_publishable_...), so the database's check is the one that counts.
import 'jsr:@supabase/functions-js/edge-runtime.d.ts'

const model = new Supabase.ai.Session('gte-small')
const problem = (status: number, message: string) => Response.json({ message }, { status })

Deno.serve(async (req) => {
  if (req.method !== 'POST') return problem(405, 'Only POST.')
  const apikey = req.headers.get('apikey')
  if (!apikey) return problem(401, 'Send the project\'s publishable key in the apikey header.')

  let body: { query?: unknown; threshold?: unknown; count?: unknown }
  try {
    body = await req.json()
  } catch {
    return problem(400, 'Send the search as JSON: { "query": "..." }.')
  }
  const query = typeof body.query === 'string' ? body.query.trim() : ''
  const threshold = body.threshold === undefined ? 0.85 : Number(body.threshold)
  const count = body.count === undefined ? 50 : Number(body.count)
  if (query.length < 3 || query.length > 500) return problem(400, 'A search needs 3 to 500 characters.')
  if (!(threshold >= -1 && threshold <= 1)) return problem(400, 'threshold is a similarity, from -1 to 1.')
  if (!(Number.isInteger(count) && count >= 1 && count <= 200)) return problem(400, 'count is 1 to 200.')

  const embedding = await model.run(query, { mean_pool: true, normalize: true })
  const reply = await fetch(`${Deno.env.get('SUPABASE_URL')}/rest/v1/rpc/match_questions`, {
    method: 'POST',
    headers: { apikey, 'Content-Type': 'application/json' },
    body: JSON.stringify({ query_embedding: embedding, match_threshold: threshold, match_count: count }),
  })
  return new Response(await reply.text(), {
    status: reply.status,
    headers: { 'Content-Type': 'application/json' },
  })
})
