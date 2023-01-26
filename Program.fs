module Program

open Reader
        
    
[<EntryPoint>]
let main argv =
    if ((argv.Length <> 2) && (argv.Length <> 3)) then
        printfn "NO"
    else if (argv.Length = 2) then
        let chsFunc = argv[0]
        let size = int (argv[1])
        handleInputForOne false chsFunc "" size
    else
        let chsFunc1 = argv[0]
        let chsFunc2 = argv[1]
        let size = int (argv[2])
        handleInputForOne true chsFunc1 chsFunc2 size
   
    // if ((argv.Length <> 4) && (argv.Length <> 5)) then
    //     printfn "NO"
    // else if (argv.Length = 4) then
    //     let chsFunc = argv[0]
    //     let a = double (argv[1])
    //     let b = double (argv[2])
    //     let size = int (argv[3])
    //     printUseFunc chsFunc
    //     let lst = handleInputForOne []
    //     let fu = chooseF chsFunc lst
    //     printUseFunc chsFunc
    //     useFuncGenerator a b size fu
    //     printfn "--------------------------------------------------"
    // else
    //     let chsFunc1 = argv[0]
    //     let chsFunc2 = argv[1]
    //     let a = double (argv[2])
    //     let b = double (argv[3])
    //     let size = int (argv[4])
    //     let lst = handleInputForOne []
    //     let useF1 = chooseF chsFunc1 lst
    //     let useF2 = chooseF chsFunc2 lst
    //     printUseFunc chsFunc1
    //     useFuncGenerator a b size useF1
    //     printfn "--------------------------------------------------"
    //     printUseFunc chsFunc2
    //     useFuncGenerator a b size useF2
    //     printfn "--------------------------------------------------"

    0
