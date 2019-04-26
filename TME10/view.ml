open Dom

let button text onclick =
  let button = Document.create_element document "button" in
  Element.set_text_content button text;
  Element.add_event_listener button "click" (fun _ -> onclick ()) false;
  button

let div elements =
  let div = Document.create_element document "div" in
  List.iter (fun element -> Element.append_child div element) elements;
  div

let input () =
  let input = Document.create_element document "input" in
  Element.set_attribute input "type" "text";
  input

let load () =
  let state = ref (Calc.create ()) in
  let screen = input () in
  let apply_and_display f =
    Element.set_value screen (string_of_float !state.x)
  in
  let calc_div =
    div [
      screen;
      button "=" (fun () -> apply_and_display Calc.apply_equal)
    ]
  in
  let body = Document.body document in
  Element.append_child body calc_div

let () =
  Window.add_event_listener window "load" (fun _ -> load ()) false


