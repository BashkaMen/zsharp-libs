open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Oxpecker

type Hello = { Hello: string }

type Item =
    {
        Id: int
        Uuid: string
        FirstName: string
        LastName: string
        Email: string
        Phone: string
        Username: string
        AvatarUrl: string
        Bio: string
        BirthDate: string
        Age: int
        Gender: string
        Company: string
        JobTitle: string
        Department: string
        Salary: float
        Currency: string
        Iban: string
        CreditScore: int
        Street: string
        City: string
        State: string
        Zip: string
        Country: string
        Latitude: float
        Longitude: float
        Timezone: string
        Locale: string
        Language: string
        IsActive: bool
        IsVerified: bool
        IsAdmin: bool
        Newsletter: bool
        LoginCount: int
        FailedLogins: int
        LastLogin: string
        CreatedAt: string
        UpdatedAt: string
        Deleted: bool
        Rating: float
        Balance: float
        Points: int
        Level: int
        Plan: string
        Status: string
        ReferralCode: string
        IpAddress: string
        UserAgent: string
        Notes: string
        Score: float
    }

let sample =
    {
        Id = 1
        Uuid = "4f9c2a1e-8b7d-4c3a-9e5f-1a2b3c4d5e6f"
        FirstName = "Alexandra"
        LastName = "Kowalski"
        Email = "alexandra.kowalski@example.com"
        Phone = "+1-202-555-0143"
        Username = "akowalski"
        AvatarUrl = "https://cdn.example.com/avatars/4f9c2a1e.png"
        Bio = "Backend engineer who enjoys distributed systems, coffee and long walks in the mountains."
        BirthDate = "1990-04-17"
        Age = 35
        Gender = "female"
        Company = "Northwind Traders"
        JobTitle = "Senior Software Engineer"
        Department = "Platform"
        Salary = 125000.5
        Currency = "USD"
        Iban = "DE89370400440532013000"
        CreditScore = 742
        Street = "1600 Pennsylvania Ave NW"
        City = "Washington"
        State = "DC"
        Zip = "20500"
        Country = "United States"
        Latitude = 38.8977
        Longitude = -77.0365
        Timezone = "America/New_York"
        Locale = "en-US"
        Language = "English"
        IsActive = true
        IsVerified = true
        IsAdmin = false
        Newsletter = false
        LoginCount = 1287
        FailedLogins = 3
        LastLogin = "2026-11-06T18:24:51Z"
        CreatedAt = "2019-02-11T09:15:00Z"
        UpdatedAt = "2026-11-05T12:00:00Z"
        Deleted = false
        Rating = 4.75
        Balance = 2389.42
        Points = 15420
        Level = 12
        Plan = "enterprise"
        Status = "active"
        ReferralCode = "AKOW-2019-X7"
        IpAddress = "203.0.113.42"
        UserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15"
        Notes = "Prefers email contact. Escalate billing questions to the account manager."
        Score = 87.3
    }

let items = Array.init 100 (fun i -> { sample with Id = i })

let hello = { Hello = "world!" }

let endpoints =
    [
        GET [
            route "/" <| text "hello world!"
            route "/json" <| json hello
            route "/object" <| json sample
            route "/list" <| json items
        ]
    ]

let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower)

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateSlimBuilder(args)
    builder.Logging.ClearProviders() |> ignore
    builder.WebHost.UseUrls("http://127.0.0.1:8084") |> ignore
    builder.Services.AddRouting().AddOxpecker().AddSingleton<IJsonSerializer>(SystemTextJsonSerializer(options)) |> ignore
    let app = builder.Build()
    app.UseRouting().UseOxpecker(endpoints) |> ignore
    app.Run()
    0
