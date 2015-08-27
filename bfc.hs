data Expr = Add Int
          | Move Int
          | Print
          | Read
          | Zero
          | LoopOpen
          | LoopClose 
          | ScanLeft
          | ScanRight 
          | Call 
          | FuncOpen
          | FuncClose deriving (Eq, Show)

parseGroup :: (Int -> Expr) -> String -> Int -> Char -> Char -> [Expr]
parseGroup f [] n o c = [f n]
parseGroup f (x:xs) n o c
    | x == o    = parseGroup f xs (n + 1) o c
    | x == c    = parseGroup f xs (n - 1) o c
    | otherwise = if n == 0 then parse (x:xs) else f n : parse (x:xs)

parse :: String -> [Expr]
parse []       = []
parse ('+':xs) = parseGroup Add xs 1 '+' '-'
parse ('-':xs) = parseGroup Add xs (-1) '+' '-'
parse ('>':xs) = parseGroup Move xs 1 '>' '<'
parse ('<':xs) = parseGroup Move xs (-1) '>' '<'
parse ('.':xs) = Print : parse xs
parse (',':xs) = Read : parse xs
parse (':':xs) = Call : parse xs
parse ('(':xs) = FuncOpen : parse xs
parse (')':xs) = FuncClose : parse xs
parse ('[':'-':']':xs) = Zero : parse xs
parse ('[':'<':']':xs) = ScanLeft : parse xs
parse ('[':'>':']':xs) = ScanRight : parse xs
parse ('[':xs) = LoopOpen : parse xs
parse (']':xs) = LoopClose : parse xs
parse (_:xs)   = parse xs

compileExprs :: Int -> [Expr] -> String
compileExprs _ []             = ""
compileExprs n ((Add x):xs)   = " a " ++ show x ++ compileExprs n xs
compileExprs n ((Move x):xs)  = " b " ++ show x ++ compileExprs n xs
compileExprs n (Print:xs)     = " c" ++ compileExprs n xs
compileExprs n (Read:xs)      = " d" ++ compileExprs n xs
compileExprs n (Zero:xs)      = " e" ++ compileExprs n xs
compileExprs n (LoopOpen:xs)  = " f" ++ compileExprs n xs
compileExprs n (LoopClose:xs) = " g" ++ compileExprs n xs
compileExprs n (ScanLeft:xs)  = " h" ++ compileExprs n xs
compileExprs n (ScanRight:xs) = " i" ++ compileExprs n xs
compileExprs n (Call:xs)      = " j" ++ compileExprs n xs
compileExprs n (FuncOpen:xs)  = " k(" ++ show n ++ ")" ++ compileExprs n xs
compileExprs n (FuncClose:xs) = " l(" ++ show n ++ ")" ++ compileExprs (n+1) xs

compile :: String -> String
compile xs =
    "#include <stdio.h>\n" ++
    "typedef void (*y)(void);\n" ++
    "#define a ;*p+=\n" ++
    "#define b ;p+=\n" ++
    "#define c ;putchar(*p)\n" ++
    "#define d ;*p=getchar()\n" ++
    "#define e ;*p=0\n" ++
    "#define f ;while(*p){\n" ++
    "#define g ;}\n" ++
    "#define h ;for(;*p!=0;--p)\n" ++
    "#define i ;for(;*p!=0;++p)\n" ++
    "#define j ;(*z[*p])()\n" ++
    "#define k(n) ;void f##n(){\n" ++
    "#define l(n) ;}z[*p]=f##n\n" ++
    "main(){int m[20000];y z[20000];register int* p=m" ++
    compileExprs 0 (parse xs) ++
    ";}"

main = do
    fileName <- getLine
    file <- readFile fileName
    putStr $ compile file

