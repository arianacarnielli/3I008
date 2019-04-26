[@@@comment "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
module Event =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x2 -> x2
    and (t_to_js : t -> Ojs.t) = fun x1 -> x1
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (client_x : t -> int) =
      fun x5 -> Ojs.int_of_js (Ojs.get (t_to_js x5) "clientX")
    let (client_y : t -> int) =
      fun x6 -> Ojs.int_of_js (Ojs.get (t_to_js x6) "clientY")
    let (page_x : t -> float) =
      fun x7 -> Ojs.float_of_js (Ojs.get (t_to_js x7) "pageX")
    let (page_y : t -> float) =
      fun x8 -> Ojs.float_of_js (Ojs.get (t_to_js x8) "pageY")
    let (screen_x : t -> int) =
      fun x9 -> Ojs.int_of_js (Ojs.get (t_to_js x9) "screenX")
    let (screen_y : t -> int) =
      fun x10 -> Ojs.int_of_js (Ojs.get (t_to_js x10) "screenY")
    let (offset_x : t -> int) =
      fun x11 -> Ojs.int_of_js (Ojs.get (t_to_js x11) "offsetX")
    let (offset_y : t -> int) =
      fun x12 -> Ojs.int_of_js (Ojs.get (t_to_js x12) "offsetY")
    let (buttons : t -> int) =
      fun x13 -> Ojs.int_of_js (Ojs.get (t_to_js x13) "buttons")
    let (alt_key : t -> bool) =
      fun x14 -> Ojs.bool_of_js (Ojs.get (t_to_js x14) "altKey")
    let (ctrl_key : t -> bool) =
      fun x15 -> Ojs.bool_of_js (Ojs.get (t_to_js x15) "ctrlKey")
    let (shift_key : t -> bool) =
      fun x16 -> Ojs.bool_of_js (Ojs.get (t_to_js x16) "shiftKey")
    let (which : t -> int) =
      fun x17 -> Ojs.int_of_js (Ojs.get (t_to_js x17) "which")
    let (code : t -> string) =
      fun x18 -> Ojs.string_of_js (Ojs.get (t_to_js x18) "code")
    let (key : t -> string) =
      fun x19 -> Ojs.string_of_js (Ojs.get (t_to_js x19) "key")
  end
module Element =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x21 -> x21
    and (t_to_js : t -> Ojs.t) = fun x20 -> x20
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (append_child : t -> t -> unit) =
      fun x25 ->
        fun x24 ->
          ignore (Ojs.call (t_to_js x25) "appendChild" [|(t_to_js x24)|])
    let (add_event_listener :
      t -> string -> (Event.t -> unit) -> bool -> unit) =
      fun x30 ->
        fun x26 ->
          fun x27 ->
            fun x29 ->
              ignore
                (Ojs.call (t_to_js x30) "addEventListener"
                   [|(Ojs.string_to_js x26);(Ojs.fun_to_js 1
                                               (fun x28 ->
                                                  x27 (Event.t_of_js x28)));(
                     Ojs.bool_to_js x29)|])
    let (get_elements_by_tag_name : t -> string -> t array) =
      fun x32 ->
        fun x31 ->
          Ojs.array_of_js t_of_js
            (Ojs.call (t_to_js x32) "getElementsByTagName"
               [|(Ojs.string_to_js x31)|])
    let (has_attribute : t -> string -> bool) =
      fun x35 ->
        fun x34 ->
          Ojs.bool_of_js
            (Ojs.call (t_to_js x35) "hasAttribute" [|(Ojs.string_to_js x34)|])
    let (get_attribute : t -> string -> string) =
      fun x37 ->
        fun x36 ->
          Ojs.string_of_js
            (Ojs.call (t_to_js x37) "getAttribute" [|(Ojs.string_to_js x36)|])
    let (set_attribute : t -> string -> string -> unit) =
      fun x40 ->
        fun x38 ->
          fun x39 ->
            ignore
              (Ojs.call (t_to_js x40) "setAttribute"
                 [|(Ojs.string_to_js x38);(Ojs.string_to_js x39)|])
    let (remove_attribute : t -> string -> unit) =
      fun x42 ->
        fun x41 ->
          ignore
            (Ojs.call (t_to_js x42) "removeAttribute"
               [|(Ojs.string_to_js x41)|])
    let (value : t -> string) =
      fun x43 -> Ojs.string_of_js (Ojs.get (t_to_js x43) "value")
    let (selected_index : t -> int) =
      fun x44 -> Ojs.int_of_js (Ojs.get (t_to_js x44) "selectedIndex")
    let (checked : t -> bool) =
      fun x45 -> Ojs.bool_of_js (Ojs.get (t_to_js x45) "checked")
    let (set_checked : t -> bool -> unit) =
      fun x46 ->
        fun x47 -> Ojs.set (t_to_js x46) "checked" (Ojs.bool_to_js x47)
    let (set_text_content : t -> string -> unit) =
      fun x48 ->
        fun x49 -> Ojs.set (t_to_js x48) "textContent" (Ojs.string_to_js x49)
    let (set_class_name : t -> string -> unit) =
      fun x50 ->
        fun x51 -> Ojs.set (t_to_js x50) "className" (Ojs.string_to_js x51)
    let (class_name : t -> string) =
      fun x52 -> Ojs.string_of_js (Ojs.get (t_to_js x52) "className")
    let (width : t -> int) =
      fun x53 -> Ojs.int_of_js (Ojs.get (t_to_js x53) "width")
    let (height : t -> int) =
      fun x54 -> Ojs.int_of_js (Ojs.get (t_to_js x54) "height")
  end
module Document =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x56 -> x56
    and (t_to_js : t -> Ojs.t) = fun x55 -> x55
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (create_element : t -> string -> Element.t) =
      fun x60 ->
        fun x59 ->
          Element.t_of_js
            (Ojs.call (t_to_js x60) "createElement"
               [|(Ojs.string_to_js x59)|])
    let (create_text_node : t -> string -> Element.t) =
      fun x62 ->
        fun x61 ->
          Element.t_of_js
            (Ojs.call (t_to_js x62) "createTextNode"
               [|(Ojs.string_to_js x61)|])
    let (get_element_by_id : t -> string -> Element.t) =
      fun x64 ->
        fun x63 ->
          Element.t_of_js
            (Ojs.call (t_to_js x64) "getElementById"
               [|(Ojs.string_to_js x63)|])
    let (get_elements_by_class_name : t -> string -> Element.t array) =
      fun x66 ->
        fun x65 ->
          Ojs.array_of_js Element.t_of_js
            (Ojs.call (t_to_js x66) "getElementsByClassName"
               [|(Ojs.string_to_js x65)|])
    let (body : t -> Element.t) =
      fun x68 -> Element.t_of_js (Ojs.get (t_to_js x68) "body")
    let (set_title : t -> string -> unit) =
      fun x69 ->
        fun x70 -> Ojs.set (t_to_js x69) "title" (Ojs.string_to_js x70)
  end
module Window =
  struct
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x72 -> x72
    and (t_to_js : t -> Ojs.t) = fun x71 -> x71
    let (t_of_js : Ojs.t -> t) = t_of_js
    let (t_to_js : t -> Ojs.t) = t_to_js
    let (add_event_listener :
      t -> string -> (Event.t -> unit) -> bool -> unit) =
      fun x79 ->
        fun x75 ->
          fun x76 ->
            fun x78 ->
              ignore
                (Ojs.call (t_to_js x79) "addEventListener"
                   [|(Ojs.string_to_js x75);(Ojs.fun_to_js 1
                                               (fun x77 ->
                                                  x76 (Event.t_of_js x77)));(
                     Ojs.bool_to_js x78)|])
    let (alert : t -> string -> unit) =
      fun x81 ->
        fun x80 ->
          ignore (Ojs.call (t_to_js x81) "alert" [|(Ojs.string_to_js x80)|])
  end
module Canvas =
  struct
    module RenderingContext2D =
      struct
        type t = Ojs.t
        let rec (t_of_js : Ojs.t -> t) = fun x83 -> x83
        and (t_to_js : t -> Ojs.t) = fun x82 -> x82
        let (set_fill_style : t -> string -> unit) =
          fun x84 ->
            fun x85 ->
              Ojs.set (t_to_js x84) "fillStyle" (Ojs.string_to_js x85)
        let (set_stroke_style : t -> string -> unit) =
          fun x86 ->
            fun x87 ->
              Ojs.set (t_to_js x86) "strokeStyle" (Ojs.string_to_js x87)
        let (clear_rect : t -> int -> int -> int -> int -> unit) =
          fun x92 ->
            fun x88 ->
              fun x89 ->
                fun x90 ->
                  fun x91 ->
                    ignore
                      (Ojs.call (t_to_js x92) "clearRect"
                         [|(Ojs.int_to_js x88);(Ojs.int_to_js x89);(Ojs.int_to_js
                                                                    x90);(
                           Ojs.int_to_js x91)|])
        let (fill_rect : t -> int -> int -> int -> int -> unit) =
          fun x97 ->
            fun x93 ->
              fun x94 ->
                fun x95 ->
                  fun x96 ->
                    ignore
                      (Ojs.call (t_to_js x97) "fillRect"
                         [|(Ojs.int_to_js x93);(Ojs.int_to_js x94);(Ojs.int_to_js
                                                                    x95);(
                           Ojs.int_to_js x96)|])
        let (line_to : t -> int -> int -> unit) =
          fun x100 ->
            fun x98 ->
              fun x99 ->
                ignore
                  (Ojs.call (t_to_js x100) "lineTo"
                     [|(Ojs.int_to_js x98);(Ojs.int_to_js x99)|])
        let (move_to : t -> int -> int -> unit) =
          fun x103 ->
            fun x101 ->
              fun x102 ->
                ignore
                  (Ojs.call (t_to_js x103) "moveTo"
                     [|(Ojs.int_to_js x101);(Ojs.int_to_js x102)|])
        let (stroke : t -> unit) =
          fun x104 -> ignore (Ojs.call (t_to_js x104) "stroke" [||])
        let (beginPath : t -> unit) =
          fun x105 -> ignore (Ojs.call (t_to_js x105) "beginPath" [||])
        let (arc : t -> int -> int -> int -> int -> float -> unit) =
          fun x111 ->
            fun x106 ->
              fun x107 ->
                fun x108 ->
                  fun x109 ->
                    fun x110 ->
                      ignore
                        (Ojs.call (t_to_js x111) "arc"
                           [|(Ojs.int_to_js x106);(Ojs.int_to_js x107);(
                             Ojs.int_to_js x108);(Ojs.int_to_js x109);(
                             Ojs.float_to_js x110)|])
      end
    type t = Ojs.t
    let rec (t_of_js : Ojs.t -> t) = fun x113 -> x113
    and (t_to_js : t -> Ojs.t) = fun x112 -> x112
    let (of_element : Element.t -> t) =
      fun x114 -> t_of_js (Element.t_to_js x114)
    let (get_context : t -> string -> Ojs.t) =
      fun x116 ->
        fun x115 ->
          Ojs.call (t_to_js x116) "getContext" [|(Ojs.string_to_js x115)|]
    let get_context_2d x = get_context x "2d"
  end
let (window : Window.t) = Window.t_of_js (Ojs.get Ojs.global "window")
let (document : Document.t) =
  Document.t_of_js (Ojs.get Ojs.global "document")