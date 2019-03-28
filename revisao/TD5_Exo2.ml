type fmark =
  |Bold
  |Italic
  |Underlined

type fchar =
  Lettre of (char * fmark list)

type fline =
  fchar list

let set_mark l a =
  if List.mem a l then
    l
  else
    a::l

let unset_mark l a =
  List.filter (fun elem -> elem <> a) l

let present_mark l a =
  List.mem a l

let change_mark l a =
  if present_mark l a then
    unset_mark l a
  else
    set_mark l a

module type INPUT =
  sig
    type param
    type input
    exception End
    val new_input : param -> input
    val input_line : input -> fline
    val close_input : input -> unit
  end

module type OUTPUT =
sig
  type param
  type output
  val new_output : param -> output
  val output_line : output -> fline -> unit
  val close_output : output -> unit
end

module type PROCESSOR =
  sig
    type input_param
    type output_param
    val process : input_param -> output_param -> unit
  end

module Processor (Input : INPUT) (Output : OUTPUT): (PROCESSOR with type input_param = Input.param and type output_param = Output.param) =
  struct
    type input_param = Input.param
    type output_param = Output.param

    let process input_param output_param =
      let input = Input.new_input input_param in
      let output = Output.new_output output_param in
      try
        while true do
          let line = Input.input_line input in
          Output.output_line output line
        done
      with
      | Input.End -> Input.close_input input; Output.close_output output
  end

module VerbatimFileInput : (INPUT with type param = string) =
  struct
    type param = string
    type input = in_channel
    exception End

    let new_input param =
      open_in param

    let input_line input =
      try
        let line = input_line input in
        let res_line = ref [] in
          for i = 0 to (String.length line) - 1 do
            res_line := Lettre(line.[i], [])::!res_line
          done;
        List.rev !res_line
      with
      End_of_file -> raise End

    let close_input input =
      close_in input
  end

module VerbatimFileOutput : (OUTPUT with type param = string) =
  struct
    type param = string
    type output = out_channel

    let new_output param =
      open_out param

    let output_line output fline =
      List.iter (
        fun (Lettre(c, _)) -> output_char output c) fline;
      output_char output '\n'

    let close_output output =
      close_out output
  end

module VerbatimProcessor = Processor (VerbatimFileInput) (VerbatimFileOutput) ;;

module MarkdownConsoleInput : (INPUT with type param = unit) =
  struct
    type param = unit
    type input = in_channel
    exception End

    let new_input param =
      stdin

    let input_line input =
      let line = input_line input in
      if line <> "q" then
        let res_line = ref [] in
        let form = ref [] in
        for i = 0 to (String.length line) - 1 do
          match line.[i] with
          |'*' -> form := change_mark !form Bold
          |'/' -> form := change_mark !form Italic
          |'_' -> form := change_mark !form Underlined
          |_ -> res_line := Lettre(line.[i], !form)::!res_line
        done;
          List.rev !res_line
      else
        raise End

    let close_input input =
      ()
  end

module HtmlConsoleOutput : (OUTPUT with type param = unit) =
  struct
    type param = unit
    type output = out_channel

    let new_output param =
      stdout

    let output_line output fline =
      List.iter (
        fun (Lettre(c, marks)) ->
          List.iter (fun (mark, print) -> if present_mark marks mark then output_string output print) [(Bold, "<b>"); (Italic, "<i>"); (Underlined, "<u>")];
          output_char output c;
          List.iter (fun (mark, print) -> if present_mark marks mark then output_string output print) [(Underlined, "</u>"); (Italic, "</i>"); (Bold, "</b>")];
      ) fline;
      output_char output '\n';
      flush output

    let close_output output =
      ()
  end

module MarkdownToHtmlProcessor = Processor (MarkdownConsoleInput) (HtmlConsoleOutput) ;;

module type INPUT_SOURCE =
sig
  type param
  type input
  exception End
  val new_input : param -> input
  val input_line : input -> string
  val close_input : input -> unit
end

module type INPUT_ANALIZER =
sig
  val analize_line : string -> fline
end

module Input (InputSource : INPUT_SOURCE) (InputAnalizer : INPUT_ANALIZER): (INPUT with type param = InputSource.param and type input = InputSource.input) =
  struct
    type param = InputSource.param
    type input = InputSource.input
    exception End = InputSource.End
    let new_input param =
      InputSource.new_input param

    let input_line input =
      let line = InputSource.input_line input in
      InputAnalizer.analize_line line

    let close_input input =
      InputSource.close_input input
  end


module FileInput : (INPUT_SOURCE with type param = string) =
struct
  type param = string
  type input = in_channel
  exception End

  let new_input param =
    open_in param

  let input_line input =
    try
      input_line input
    with
      End_of_file -> raise End

  let close_input input =
    close_in input
end

module ConsoleInput : (INPUT_SOURCE with type param = unit) =
struct
  type param = unit
  type input = in_channel
  exception End

  let new_input param =
    stdin

  let input_line input =
    let line = input_line input in
    if line <> "q" then
      line
    else
      raise End

  let close_input input =
    ()
end

module VerbatimInput : (INPUT_ANALIZER) =
  struct
    let analize_line line =
      let res_line = ref [] in
        for i = 0 to (String.length line) - 1 do
          res_line := Lettre(line.[i], [])::!res_line
        done;
      List.rev !res_line
  end

module MarkdownInput : (INPUT_ANALIZER) =
  struct
    let analize_line line =
      let res_line = ref [] in
      let form = ref [] in
      for i = 0 to (String.length line) - 1 do
        match line.[i] with
        |'*' -> form := change_mark !form Bold
        |'/' -> form := change_mark !form Italic
        |'_' -> form := change_mark !form Underlined
        |_ -> res_line := Lettre(line.[i], !form)::!res_line
      done;
        List.rev !res_line
  end



module MarkdownFileInput = Input (FileInput) (MarkdownInput)
module TestInputProcessor = Processor (MarkdownFileInput) (HtmlConsoleOutput) ;;

module type OUTPUT_DEST =
  sig
    type param
    type output
    val new_output : param -> output
    val output_line : output -> string -> unit
    val close_output : output -> unit
  end


module type OUTPUT_RENDERER =
  sig
    val renderer_line : fline -> string
  end

module Output (OutputSource : OUTPUT_DEST) (OutputRenderer : OUTPUT_RENDERER): (OUTPUT with type param = OutputSource.param and type output = OutputSource.output) =
  struct
    type param = OutputSource.param
    type output = OutputSource.output

    let new_output param =
      OutputSource.new_output param

    let output_line output fline =
      let line = OutputRenderer.renderer_line fline in
      OutputSource.output_line output line

    let close_output output =
      OutputSource.close_output output
  end

module FileDest : (OUTPUT_DEST with type param = string) =
  struct
    type param = string
    type output = out_channel

    let new_output param =
      open_out param

    let output_line output line =
      output_string output line;
      output_char output '\n'

    let close_output output =
      close_out output
  end

module ConsoleDest : (OUTPUT_DEST with type param = unit) =
  struct
    type param = unit
    type output = out_channel

    let new_output param =
      stdout

    let output_line output line =
      output_string output line;
      output_char output '\n';
      flush output

    let close_output output =
      ()
  end


module VerbatimRenderer : (OUTPUT_RENDERER) =
  struct
    let renderer_line fline =
      List.fold_left (fun acc (Lettre(c, _)) -> acc^(Char.escaped c)) "" fline
  end

module HtmlRenderer : (OUTPUT_RENDERER) =
  struct
    (*let renderer_line fline =
      List.fold_left (
        fun acc (Lettre (c, marks)) ->
          List.fold_left (fun acc2 (mark, print) -> if present_mark marks mark then acc2^print else acc2) "" [(Bold, "<b>"); (Italic, "<i>"); (Underlined, "<u>")]
      ) "" fline
    *)
    let renderer_line fline =
      let rec loop acc fline =
        let l_marks1 = [|(Bold, "<b>"); (Italic, "<i>"); (Underlined, "<u>")|] in
        let l_marks2 = [|(Underlined, "</u>"); (Italic, "</i>"); (Bold, "</b>")|] in
        match fline with
        |[] -> acc
        |hd::tl ->
          match hd with
          |Lettre(c, marks)->
            let acc2 = ref "" in
            for i = 0 to (Array.length l_marks1) - 1 do
              match l_marks1.(i) with
              |(mark, tag) ->
                if present_mark marks mark then
                  acc2 := (!acc2^tag)
            done;
            acc2 := (!acc2^(Char.escaped c));
            for i = 0 to (Array.length l_marks2) - 1 do
              match l_marks2.(i) with
              |(mark, tag) ->
                if present_mark marks mark then
                  acc2 := (!acc2^tag)
            done;
          loop (acc^(!acc2)) tl
      in loop "" fline
  end

module MarkdownFileInput = Input (FileInput) (MarkdownInput)
module HtmlOutput = Output (ConsoleDest) (HtmlRenderer)
module HtmlOutput = Output (FileDest) (HtmlRenderer)
module TestInputProcessor = Processor (MarkdownFileInput) (HtmlOutput) ;;


TestInputProcessor.process "exemplo1.txt" "resultado1.txt";;

(*VerbatimProcessor.process Sys.argv.(1) Sys.argv.(2);;*)
