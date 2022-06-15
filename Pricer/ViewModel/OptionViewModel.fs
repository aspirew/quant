namespace ViewModel

type OptionViewModel(input : OptionRecord) = 
    inherit ViewModelBase()

    let mutable userInput = input
    let mutable delta1: float = 0.0
    let mutable delta2: float = 0.0
    let mutable call: float = 0.0
    let mutable put: float = 0.0

    member this.StockPrice
        with get() = userInput.StockPrice
        and set(x) = 
            userInput <- {userInput with StockPrice = x }
            base.Notify("StockPrice")

    member this.Strike
        with get() = userInput.Strike
        and set(x) = 
            userInput <- {userInput with Strike = x }
            base.Notify("Strike")

    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.risklessRate 
        with get() = userInput.risklessRate
        and set(x) = 
            userInput <- {userInput with risklessRate = x }
            base.Notify("risklessRate")

    member this.volatility
        with get() = userInput.volatility
        and set(x) = 
            userInput <- {userInput with volatility = x }
            base.Notify("volatility")

    member this.Delta1
        with get() = delta1
        and set(x) = 
            delta1 <- x
            base.Notify("Delta1")

    member this.Delta2 
        with get() = delta2
        and set(x) = 
            delta2 <- x
            base.Notify("Delta2")

    member this.Call 
        with get() = call
        and set(x) = 
            call <- x
            base.Notify("Call")

    member this.Put
        with get() = put
        and set(x) = 
            put <- x
            base.Notify("Put")

    member this.CalculateOption() = 
        
        //capture inputs
        let optionParameters : OptionParameters = 
            {
                StockPrice = this.StockPrice
                Strike = this.Strike
                Expiry = this.Expiry
                risklessRate = this.risklessRate
                volatility = this.volatility
            }
        //calculate
        let calc = OptionEvaluation.BlackScholes(optionParameters)

        //present to the user
        this.Delta1 <- calc.Delta1
        this.Delta2 <- calc.Delta2
        this.Call <- calc.Call
        this.Put <- calc.Put