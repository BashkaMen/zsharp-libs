var builder = WebApplication.CreateSlimBuilder(args);
builder.Logging.ClearProviders();
builder.WebHost.UseUrls("http://127.0.0.1:8083");

var app = builder.Build();

app.MapGet("/", () => "hello world!");
app.MapGet("/json", () => Results.Json(new Hello("world!"), HelloContext.Default.Hello));
app.MapGet("/object", () => Results.Json(Data.Sample, HelloContext.Default.Item));
app.MapGet("/list", () => Results.Json(Data.Items, HelloContext.Default.ItemArray));

app.Run();

record Hello(string hello);

[System.Text.Json.Serialization.JsonSourceGenerationOptions(PropertyNamingPolicy = System.Text.Json.Serialization.JsonKnownNamingPolicy.SnakeCaseLower)]
[System.Text.Json.Serialization.JsonSerializable(typeof(Hello))]
[System.Text.Json.Serialization.JsonSerializable(typeof(Item))]
[System.Text.Json.Serialization.JsonSerializable(typeof(Item[]))]
partial class HelloContext : System.Text.Json.Serialization.JsonSerializerContext;
