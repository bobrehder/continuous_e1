var express = require("express")
var app = express()
app.use(express.static (__dirname + "/public"))
app.engine("html", require("ejs").renderFile)
app.set("view engine","html")
app.get ("/", function (request, response) {
  response.render ("index.html")
})

var server = app.listen (3000, function () {
  console.log ("listening to port %d", server.address().port)
})

