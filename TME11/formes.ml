open Dom

let draw () =
  let mon_canvas = Document.get_element_by_id document "mon_canvas" in
  let _ = Element.set_attribute mon_canvas "width" "300" in
  let _ = Element.set_attribute mon_canvas "height" "400" in
  let canvas = Canvas.of_element mon_canvas in
  let ctx_canvas = Canvas.get_context_2d canvas in
  let _ = Canvas.RenderingContext2D.set_fill_style ctx_canvas "red" in
  let _ = Canvas.RenderingContext2D.fill_rect ctx_canvas 0 0 50 50 in
  let _ = Canvas.RenderingContext2D.beginPath ctx_canvas in
  let _ = Canvas.RenderingContext2D.move_to ctx_canvas 0 0 in
  let _ = Canvas.RenderingContext2D.line_to ctx_canvas 300 400 in
  let _ =   Canvas.RenderingContext2D.stroke ctx_canvas in
  let _ = Canvas.RenderingContext2D.beginPath ctx_canvas in
  let _ = Canvas.RenderingContext2D.arc ctx_canvas 100 100 50 0 (2. *. 3.14) in
  Canvas.RenderingContext2D.stroke ctx_canvas






let _ = draw ()
