"start" { wait for any then open "src/Lib.hs" "haskell" }
"get the symbols" {
  wait for
    method == "textDocument/publishDiagnostics"
  then
    open "src/Lib.hs" "haskell"
    id1: request "textDocument/documentSymbol" {
      textDocument: {
        uri: uri "src/Lib.hs"
      }
    }
}
"check the symbols" {
  wait for
    id == 1
  then
    open "src/Lib.hs" "haskell"
}