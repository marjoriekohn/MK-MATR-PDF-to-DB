# Page building blocks that hold no markup of their own (helpers_layout.R).

test_that("a steady reactive tells what reads it only when its value changes, and keeps the last while it can't say", {
  shiny::testServer(function(input, output, session) {
    reread <- shiny::reactiveVal(0L) # as the drafts are read again every few seconds
    words <- shiny::reactiveVal("a")
    open <- shiny::reactiveVal(TRUE)
    steady <- steady_reactive(function() {
      shiny::req(open())
      reread()
      words()
    })
    drawn <- new.env() # (an environment: the test below reads the count as it is then, not a copy)
    drawn$times <- 0L
    output$shown <- shiny::renderText({
      drawn$times <- drawn$times + 1L
      steady()
    })
  }, {
    expect_equal(output$shown, "a")
    first <- drawn$times
    reread(1L) # read again, the same
    session$flushReact()
    expect_equal(drawn$times, first) # not drawn again
    words("b")
    session$flushReact()
    expect_equal(output$shown, "b")
    expect_equal(drawn$times, first + 1L)
    open(FALSE) # can't say: the last stays
    session$flushReact()
    expect_equal(steady(), "b")
  })
})
