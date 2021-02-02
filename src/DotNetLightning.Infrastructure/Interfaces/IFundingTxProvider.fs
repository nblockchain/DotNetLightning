namespace DotNetLightning.Infrastructure

open DotNetLightning.Utils
open DotNetLightning.Transactions
open NBitcoin

open ResultUtils.Portability

type IFundingTxProvider =
    abstract member ProvideFundingTx: IDestination * Money * FeeRatePerKw -> Result<FinalizedTx * TxOutIndex, string> 
