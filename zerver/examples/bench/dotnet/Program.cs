var builder = WebApplication.CreateSlimBuilder(args);
builder.Logging.ClearProviders();
builder.WebHost.UseUrls("http://127.0.0.1:8083");

var app = builder.Build();

app.MapGet("/", () => "hello world!");
app.MapGet("/json", () => Results.Json(new Hello("world!"), HelloContext.Default.Hello));

app.Run();

record Hello(string hello);

[System.Text.Json.Serialization.JsonSerializable(typeof(Hello))]
partial class HelloContext : System.Text.Json.Serialization.JsonSerializerContext;
