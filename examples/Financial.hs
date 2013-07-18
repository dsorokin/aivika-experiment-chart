
{-# LANGUAGE RecursiveDo #-}

-- This financial model is described in
-- Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.
--
-- It illustrates how you can use the Monte-Carlo simulation
-- and define external parameters. Here the system of recursive
-- diffential equations is used but the paradigm can be any
-- supported by Aivika including DES or agent-base modeling
-- or their combination.
--
-- To enable the parallel simulation, you should compile it
-- with option -threaded and then pass in other options +RTS -N2 -RTS
-- to the executable if you have a dual core processor without
-- hyper-threading. Also you can increase the number
-- of parallel threads via option -N if you have a more modern
-- processor.

import Control.Monad

-- from package aivika
import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.SystemDynamics
import Simulation.Aivika.Dynamics.Parameter
import Simulation.Aivika.Dynamics.EventQueue

-- from package aivika-experiment
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.FinalStatsView

-- from package aivika-experiment-chart
import Simulation.Aivika.Experiment.DeviationChartView
import Simulation.Aivika.Experiment.FinalHistogramView
import Simulation.Aivika.Experiment.TimeSeriesView

-- | The model parameters.
data Parameters =
  Parameters { paramsTaxDepreciationTime    :: Simulation Double,
               paramsTaxRate                :: Simulation Double,
               paramsAveragePayableDelay    :: Simulation Double,
               paramsBillingProcessingTime  :: Simulation Double,
               paramsBuildingTime           :: Simulation Double,
               paramsDebtFinancingFraction  :: Simulation Double,
               paramsDebtRetirementTime     :: Simulation Double,
               paramsDiscountRate           :: Simulation Double,
               paramsFractionalLossRate     :: Simulation Double,
               paramsInterestRate           :: Simulation Double,
               paramsPrice                  :: Simulation Double,
               paramsProductionCapacity     :: Simulation Double,
               paramsRequiredInvestment     :: Simulation Double,
               paramsVariableProductionCost :: Simulation Double }

-- | The default model parameters.
defaultParams :: Parameters
defaultParams =
  Parameters { paramsTaxDepreciationTime    = 10,
               paramsTaxRate                = 0.4,
               paramsAveragePayableDelay    = 0.09,
               paramsBillingProcessingTime  = 0.04,
               paramsBuildingTime           = 1,
               paramsDebtFinancingFraction  = 0.6,
               paramsDebtRetirementTime     = 3,
               paramsDiscountRate           = 0.12,
               paramsFractionalLossRate     = 0.06,
               paramsInterestRate           = 0.12,
               paramsPrice                  = 1,
               paramsProductionCapacity     = 2400,
               paramsRequiredInvestment     = 2000,
               paramsVariableProductionCost = 0.6 }

-- | Random parameters for the Monte-Carlo simulation.
randomParams :: IO Parameters
randomParams =
  do averagePayableDelay    <- newRandomParameter 0.07 0.11
     billingProcessingTime  <- newRandomParameter 0.03 0.05
     buildingTime           <- newRandomParameter 0.8 1.2
     fractionalLossRate     <- newRandomParameter 0.05 0.08
     interestRate           <- newRandomParameter 0.09 0.15
     price                  <- newRandomParameter 0.9 1.2
     productionCapacity     <- newRandomParameter 2200 2600
     requiredInvestment     <- newRandomParameter 1800 2200
     variableProductionCost <- newRandomParameter 0.5 0.7
     return defaultParams { paramsAveragePayableDelay    = averagePayableDelay,
                            paramsBillingProcessingTime  = billingProcessingTime,
                            paramsBuildingTime           = buildingTime,
                            paramsFractionalLossRate     = fractionalLossRate,
                            paramsInterestRate           = interestRate,
                            paramsPrice                  = price,
                            paramsProductionCapacity     = productionCapacity,
                            paramsRequiredInvestment     = requiredInvestment,
                            paramsVariableProductionCost = variableProductionCost }

-- | This is the model itself that returns experimental data.
model :: Parameters -> Simulation ExperimentData
model params =
  mdo let liftParam :: (Parameters -> Simulation a) -> Dynamics a
          liftParam f = liftSimulation $ f params

      -- the equations below are given in an arbitrary order!

      bookValue <- integ (newInvestment - taxDepreciation) 0
      let taxDepreciation = bookValue / taxDepreciationTime
          taxableIncome = grossIncome - directCosts - losses
                          - interestPayments - taxDepreciation
          production = availableCapacity
          availableCapacity = ifDynamics (time .>=. buildingTime)
                              productionCapacity 0
          taxDepreciationTime = liftParam paramsTaxDepreciationTime
          taxRate = liftParam paramsTaxRate
      accountsReceivable <- integ (billings - cashReceipts - losses)
                            (billings / (1 / averagePayableDelay
                                         + fractionalLossRate))
      let averagePayableDelay =
            liftParam paramsAveragePayableDelay
      awaitingBilling <- integ (price * production - billings)
                         (price * production * billingProcessingTime)
      let billingProcessingTime =
            liftParam paramsBillingProcessingTime
          billings = awaitingBilling / billingProcessingTime
          borrowing = newInvestment * debtFinancingFraction
          buildingTime = liftParam paramsBuildingTime
          cashReceipts = accountsReceivable / averagePayableDelay
      debt <- integ (borrowing - principalRepayment) 0
      let debtFinancingFraction = liftParam paramsDebtFinancingFraction
          debtRetirementTime = liftParam paramsDebtRetirementTime
          directCosts = production * variableProductionCost
          discountRate = liftParam paramsDiscountRate
          fractionalLossRate = liftParam paramsFractionalLossRate
          grossIncome = billings
          interestPayments = debt * interestRate
          interestRate = liftParam paramsInterestRate
          losses = accountsReceivable * fractionalLossRate
          netCashFlow = cashReceipts + borrowing - newInvestment
                        - directCosts - interestPayments
                        - principalRepayment - taxes
          netIncome = taxableIncome - taxes
          newInvestment = ifDynamics (time .>=. buildingTime)
                          0 (requiredInvestment / buildingTime)
      npvCashFlow <- npv netCashFlow discountRate 0 1
      npvIncome <- npv netIncome discountRate 0 1
      let price = liftParam paramsPrice
          principalRepayment = debt / debtRetirementTime
          productionCapacity = liftParam paramsProductionCapacity
          requiredInvestment = liftParam paramsRequiredInvestment
          taxes = taxableIncome * taxRate
          variableProductionCost = liftParam paramsVariableProductionCost

      -- we have to create an event queue to return the experimental data,
      -- although it was not used in the model itself 

      queue <- newQueue
      
      experimentDataInStartTime queue
        [(netIncomeName, seriesEntity "Net income" netIncome),
         (netCashFlowName, seriesEntity "Net cash flow" netCashFlow),
         (npvIncomeName, seriesEntity "NPV income" npvIncome),
         (npvCashFlowName, seriesEntity "NPV cash flow" npvCashFlow)]

-- the names of the variables we are interested in
netIncomeName   = "netIncome"
netCashFlowName = "netCashFlow"
npvIncomeName   = "npvIncome"
npvCashFlowName = "npvCashFlow"

-- the simulation specs
specs = Specs 0 5 0.015625 RungeKutta4

-- | The experiment for the Monte-Carlo simulation.
monteCarloExperiment :: Experiment
monteCarloExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1000,
    experimentTitle = "Financial Model (the Monte-Carlo simulation)",
    experimentDescription = "Financial Model (the Monte-Carlo simulation) as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Income and Cash Flow",
         deviationChartSeries = [Left netIncomeName, 
                                 Left netCashFlowName] },

       outputView $ defaultDeviationChartView {
         deviationChartTitle = "The deviation chart for Net Present Value of Income and Cash Flow",
         deviationChartSeries = [Left npvIncomeName, 
                                 Left npvCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Income and Cash Flow",
         finalHistogramSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalHistogramView {
         finalHistogramTitle = "Histogram for Net Present Value of Income and Cash Flow",
         finalHistogramSeries = [npvIncomeName, npvCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Income and Cash Flow",
         finalStatsSeries = [netIncomeName, netCashFlowName] },

       outputView $ defaultFinalStatsView {
         finalStatsTitle = "Summary for Net Present Value of Income and Cash Flow",
         finalStatsSeries = [npvIncomeName, npvCashFlowName] } ] }

-- | The experiment with single simulation run.
singleExperiment :: Experiment
singleExperiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentTitle = "Financial Model",
    experimentDescription = "Financial Model as described in " ++
                            "Vensim 5 Modeling Guide, Chapter Financial Modeling and Risk.",
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Income and Cash Flow",
         timeSeries = [Left netIncomeName, 
                       Left netCashFlowName] },
       
       outputView $ defaultTimeSeriesView {
         timeSeriesTitle = "Time series of Net Present Value for Income and Cash Flow",
         timeSeries = [Left npvIncomeName, 
                       Left npvCashFlowName] },

       outputView $ defaultTableView {
         tableTitle = "Table",
         tableSeries = [netIncomeName, netCashFlowName,
                        npvIncomeName, npvCashFlowName] } ] }

main = do
  
  -- run the ordinary simulation
  putStrLn "*** The simulation with default parameters..."
  runExperiment singleExperiment $ model defaultParams
  putStrLn ""

  -- run the Monte-Carlo simulation
  putStrLn "*** The Monte-Carlo simulation..."
  randomParams >>= runExperimentParallel monteCarloExperiment . model