type fmark = Bold|Italic|Underlined
type fchar = (char * fmark list)
type fline = fchar list

let set_mark l a = a::l

let unset_mark l a = List.filter(fun m -> m <> a) l

let present_mark l a = List.mem a l

let change_mark l a = if present_mark l a then
    unset_mark l a
  else
    set_mark l a

module type INPUT =
sig
  type t
  type param
  exception End

  val new_input: param -> t
  val input_line: t -> fline
  val close_input: t -> unit
end

module type OUTPUT =
sig
  type param
  type t

  val new_output: param -> t
  val output_line: t -> fline -> unit
  val close_output: t -> unit
end

module type PROCESSOR =
sig
  type input_param
  type output_param
  val process: input_param -> output_param -> unit
end

module Processor (Input: INPUT) (Output: OUTPUT): (PROCESSOR with type input_param = Input.param and type output_param = Output.param) =
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
        |Input.End -> Input.close_input input;
          Output.close_output output
end

module VerbatimFileInput: (INPUT with type param = string) =
struct
  type param = string
  type t = in_channel
  exception End

  let new_input f = open_in f

  let input_line ic =
    let s = try input_line ic with End_of_file -> raise End in
    let rec loop i =
          if i = String.length s then
            []
          else
            (s.[i], [])::loop(i+1) in
    loop 0

  let close_input f = close_in f
end

module VerbatimFileOutput: (OUTPUT with type param = string) =
struct
  type param = string
  type t = out_channel

  let new_output f = open_out f

  let output_line oc fl =
    List.iter(fun (c, _) -> output_char oc c) fl

  let close_output f = close_out f
end

module VerbatimProcessor = Processor(VerbatimFileInput)(VerbatimFileOutput)

let _ =  VerbatimProcessor.process Sys.argv.(1) Sys.argv.(2)












(*dhgy*)
