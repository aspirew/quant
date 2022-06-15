namespace ViewModel

type OptionParameters =
    {
        StockPrice: float
        Strike: float
        Expiry: System.DateTime
        risklessRate: float
        volatility: float
    }

type OptionEvaluation =
    {
        Delta1: float
        Delta2: float
        Call: float
        Put: float
    }

    static member BlackScholes(parameters: OptionParameters): OptionEvaluation =
        let S = parameters.StockPrice
        let K = parameters.Strike
        let T = 
            let daysDiff = (parameters.Expiry - System.DateTime.Now).TotalDays
            match daysDiff with
            | x when x <= 0.0 -> 0.0
            | y -> y / 365.0
        let r = parameters.risklessRate
        let v = parameters.volatility

        if(T <= 0.0) then
            {
                Delta1 = 0.0
                Delta2 = 0.0
                Call = 0.0
                Put = 0.0
            }
        else
            let d1 = (log(S / K) + (r + (pown v 2)) * T) / (v * sqrt T)
            let d2 = d1 - v * sqrt T
            let NormalCDF = FSharp.Stats.Distributions.Continuous.Normal.CDF 0.0 1.0
            let c = S * NormalCDF(d1) - K * exp(-r*T) * NormalCDF(d2)
            let p = K * exp(-r*T) * NormalCDF(-d2) - S * NormalCDF(-d1)
            {
                Delta1 = round (d1  * 1000.) / 1000.
                Delta2 = round (d2 * 1000.) / 1000.
                Call = round (c * 1000.) / 1000.
                Put = round (p * 1000.) / 1000.
            }

type OptionRecord =
    {
        StockPrice: float
        Strike: float
        Expiry: System.DateTime
        risklessRate: float
        volatility: float
        Delta1: float
        Delta2: float
        Call: float
        Put: float
    }
    static member DefaultValues() = 
        let calc = OptionEvaluation.BlackScholes({
            StockPrice = 10.0
            Strike = 10.0
            Expiry = System.DateTime.Now
            risklessRate = 0.04
            volatility = 0.3
        })

        {
            StockPrice = 10.0
            Strike = 10.0
            Expiry = System.DateTime.Now
            risklessRate = 0.04
            volatility = 0.3
            Delta1 = calc.Delta1
            Delta2 = calc.Delta2
            Call = calc.Call
            Put = calc.Put
        }



