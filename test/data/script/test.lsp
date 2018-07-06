"start" { wait for any then open "Test.hs" "haskell" }
"get the symbols" {
  wait for
    method == "textDocument/publishDiagnostics"
  then
    open "Test.hs" "haskell"
    id1: request "textDocument/documentSymbol" {
      textDocument: {
        uri: uri "Test.hs"
      }
    }
}
"check the symbols" {
  wait for
    id == 1
  then
    open "Test.hs" "haskell"
}
