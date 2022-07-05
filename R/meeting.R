init_meeting <- function() {
  meeting <<- new_meeting()
}

new_meeting <- function() {
  output <- list(
    decisions = list(),
    action_points = list()
  )

  class(output) <- c("meeting", class(output))
  output
}

decision <- function(text) {
  meeting$decisions <<- append(meeting$decisions, text)
  text
}


action <- function(action_text, initials) {
  meeting$action_points <<- append(meeting$action_points, list(list(action = action_text, owner = initials)))
  output <- paste0(action_text, ' <div class="sidenote">', eval_attendee_vector(initials), '</div>')
  output
}


show_decisions <- function() {
  tibble::tibble(decisions = meeting$decisions) |>
    gt::gt()
}

show_action_points <- function()
  tibble::tibble(action = purrr::map_chr(meeting$action_points, "action"),
                 owner = purrr::map(meeting$action_points, "owner") |> purrr::map(eval_attendee_vector)) |>
  gt::gt() |>
  gt::fmt_passthrough(columns = "owner",
                      escape = FALSE)
