module Gto = GotoAST
module Tmp = CFGTemp
open CommonAST

let rec translate_instruction instr next = match instr with
  | Gto.Sequence(i1,i2) ->
     let i = translate_instruction i2 next
     in
     translate_instruction i1 i
  | Gto.Print(e) ->
     Tmp.Print(e,next)
  | Gto.Set(l,e) ->
     Tmp.Set(l,e,next)
  | Gto.Label(l) ->
     HashTbl.add labelTblsk l next
       next
  | Gto.Goto(l) ->
     Tmp.Goto(Label(l))
  | Gto.ConditionalGoto(l,e) ->
     Tmp.ConditionalGoto(e,Label(l),next)
  | Gto.Nop ->
     next

let rec clean_instruction = function
  | Tmp.Print(e,i) ->
     Tmp.Print(e, clean_instruction i)
  | Tmp.Set(l,e,i) ->
     Tmp.Set(l,e, clean_instruction i)
  | Tmp.ConditionalGoto(e,lbl,i2) ->
     Tmp.ConditionalGoto(e, clean_lbl lbl, clean_instruction i2)
  | Tmp.Goto(lbl) ->
     clean_lbl lbl
  | Tmp.Nop ->
     Tmp.Nop

and clean_lbl = function
  | Instruction(i) ->
     clean_instruction i
  | Label(l) ->
     HashTbl.find labelTblsk l
