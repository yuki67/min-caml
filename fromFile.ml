(* ファイル名を受け取り、コンパイルを各フェーズまで進める関数 *)

(* コンパイルだけ *)
let syntax filename =
  let outchan = open_in filename in
  (* エラーの解析のためにbufをとっておく必要がある *)
  let buf = Lexing.from_channel outchan in
  try Parser.exp Lexer.token buf
  with Error.ParseError (start, end_) ->
    Error.explain_parse_error buf start end_
let type_    str = syntax   str |> Typing.f
let knormal  str = type_    str |> KNormal.f
let alpha    str = knormal  str |> Alpha.f
let closure  str = alpha    str |> Closure.f
let virtual_ str = closure  str |> Virtual.f
let simm     str = virtual_ str |> Simm.f
let regalloc str = simm     str |> RegAlloc.f
let emit     str = regalloc str |> Emit.f stdout

(* コンパイルして出力する *)
let syntax_p   str = syntax   str |> Syntax.print
let type_p     str = type_    str |> Syntax.print
let knormal_p  str = knormal  str |> KNormal.print
let alpha_p    str = alpha    str |> KNormal.print
let closure_p  str = closure  str |> Closure.print
let virtual_p  str = virtual_ str |> Asm.print
let simm_p     str = simm     str |> Asm.print
let regalloc_p str = regalloc str |> Asm.print
