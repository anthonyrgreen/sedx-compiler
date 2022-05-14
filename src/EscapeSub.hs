module EscapeSub
  ( escapeLinkedSub
  ) where


import           Control.Monad.Trans.Free
import           Control.Monad.Writer.Lazy
import           ProgramAst


-- TODO: use custom separator char (we're using '/' implicitly)
escapeLinkedSub :: LinkedSub () -> String
escapeLinkedSub = execWriter . iterM processLine
  where
    processLine :: LinkedSubF (Writer String a) -> Writer String a
    processLine (LinkedSubLiteral literal next) = tell (escapeLiteral literal) >> next
    processLine (LinkedSubBackReference captureGroupNum next) = tell ("\\" ++ show captureGroupNum) >> next

escapeLiteral = concatMap escapeChar
  where
    escapeChar '/'  = "\\/"
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]
