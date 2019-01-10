﻿module internal NBomber.Domain.StatisticsTypes

open System
open NBomber.Contracts
open NBomber.Domain.DomainTypes

[<Measure>] type Bytes
[<Measure>] type Kb
[<Measure>] type MB

type DataTransferCount = {
    MinKb: float<Kb>
    MeanKb: float<Kb>
    MaxKb: float<Kb>
    AllMB: float<MB>
}

type LatencyCount = {
    Less800: int
    More800Less1200: int
    More1200: int
}

type StepResults = {     
    StepName: string
    Results: (Response*Latency)[]    
    DataTransferCount: DataTransferCount
}

type StepStats = {
    StepName: string
    OkLatencies: Latency[]
    ReqeustCount: int    
    OkCount: int
    FailCount: int   
    RPS: int
    Min: Latency
    Mean: Latency
    Max: Latency
    Percent50: Latency
    Percent75: Latency
    Percent95: Latency
    StdDev: int
    DataTransferCount: DataTransferCount
}

type ScenarioStats = {
    ScenarioName: string    
    StepsStats: StepStats[]
    ConcurrentCopies: int    
    OkCount: int
    FailCount: int
    LatencyCount: LatencyCount
    ActiveTime: TimeSpan
    Duration: TimeSpan
}

type GlobalStats = {
    AllScenariosStats: ScenarioStats[]
    OkCount: int
    FailCount: int
    LatencyCount: LatencyCount    
}