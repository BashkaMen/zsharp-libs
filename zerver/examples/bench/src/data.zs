open Zjson

[<Derive(ToJson, Serialize)>]
type Item = {
    id: int
    uuid: string
    first_name: string
    last_name: string
    email: string
    phone: string
    username: string
    avatar_url: string
    bio: string
    birth_date: string
    age: int
    gender: string
    company: string
    job_title: string
    department: string
    salary: f64
    currency: string
    iban: string
    credit_score: int
    street: string
    city: string
    state: string
    zip: string
    country: string
    latitude: f64
    longitude: f64
    timezone: string
    locale: string
    language: string
    is_active: bool
    is_verified: bool
    is_admin: bool
    newsletter: bool
    login_count: int
    failed_logins: int
    last_login: string
    created_at: string
    updated_at: string
    deleted: bool
    rating: f64
    balance: f64
    points: int
    level: int
    plan: string
    status: string
    referral_code: string
    ip_address: string
    user_agent: string
    notes: string
    score: f64
}

let sample : Item = {
    id = 1
    uuid = "4f9c2a1e-8b7d-4c3a-9e5f-1a2b3c4d5e6f"
    first_name = "Alexandra"
    last_name = "Kowalski"
    email = "alexandra.kowalski@example.com"
    phone = "+1-202-555-0143"
    username = "akowalski"
    avatar_url = "https://cdn.example.com/avatars/4f9c2a1e.png"
    bio = "Backend engineer who enjoys distributed systems, coffee and long walks in the mountains."
    birth_date = "1990-04-17"
    age = 35
    gender = "female"
    company = "Northwind Traders"
    job_title = "Senior Software Engineer"
    department = "Platform"
    salary = 125000.5
    currency = "USD"
    iban = "DE89370400440532013000"
    credit_score = 742
    street = "1600 Pennsylvania Ave NW"
    city = "Washington"
    state = "DC"
    zip = "20500"
    country = "United States"
    latitude = 38.8977
    longitude = -77.0365
    timezone = "America/New_York"
    locale = "en-US"
    language = "English"
    is_active = true
    is_verified = true
    is_admin = false
    newsletter = false
    login_count = 1287
    failed_logins = 3
    last_login = "2026-11-06T18:24:51Z"
    created_at = "2019-02-11T09:15:00Z"
    updated_at = "2026-11-05T12:00:00Z"
    deleted = false
    rating = 4.75
    balance = 2389.42
    points = 15420
    level = 12
    plan = "enterprise"
    status = "active"
    referral_code = "AKOW-2019-X7"
    ip_address = "203.0.113.42"
    user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_5) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15"
    notes = "Prefers email contact. Escalate billing questions to the account manager."
    score = 87.3
}

let items : list<Item> = List.init 100 (fun i -> { sample with id = i })
