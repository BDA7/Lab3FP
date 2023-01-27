module Program

open Reader
        
    
[<EntryPoint>]
let main argv =
    if ((argv.Length <> 2) && (argv.Length <> 3)) then
        printfn "NO"
    else if (argv.Length = 2) then
        let chsFunc = argv[0]
        let size = int (argv[1])
        handleInputForOne chsFunc "" size
    else
        let chsFunc1 = argv[0]
        let chsFunc2 = argv[1]
        let size = int (argv[2])
        handleInputForOne chsFunc1 chsFunc2 size
    0
