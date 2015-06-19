var https = require('https');
var sockjs = require('sockjs');
var express = require('express');
var session = require('express-session');
var bodyParser = require('body-parser');
var storage = require('./storage.js');
var globalSession = require('./session.js');
var rest = require('./rest.js');
var ws = require('./ws.js');
var rtc = require('./rtc.js');
var fs = require('fs');
var uuid = require('uuid');

var authenticate = function(user) {
  return true;
};

var app = express();
var sessionHandler = session({
  name: 'elm-video-chat',
  secret: 'secret',
  cookie: { secure: true }
});
app.use(sessionHandler);
app.use(bodyParser());


storage.addUser('foo@gmail.com', 'Foo', 'foo@gmail.com');
storage.addUser('bar@gmail.com', 'Bar', 'bar@gmail.com');


var loginCheck = function(req, res, next) {
  if (req.method === 'POST' && req.url === '/api/login') {
    var email = req.body.email;
    var password = req.body.password;
    req.session.user = email;
    res.redirect(req.header('referrer'));
  } else if (req.method === 'GET' && req.url === '/logout') {
    req.session.destroy();
    console.log('deleted sesstion');
    res.redirect('/');
  } else {
    if (req.session.user) {
      next();
    } else {
      if(req.url === '/') {
        req.url = '/login.html';
      }
      staticRouter(req, res, next);
    }
  }
};
app.use(loginCheck);

// setup static
var staticRouter = express.static(__dirname + '/../public');
app.use(staticRouter);

// setup REST
rest(app, staticRouter, storage, globalSession);

var server = https.createServer({
  key: fs.readFileSync(__dirname + '/ssl/key.pem'),
  cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
}, app);

// setup websocket
var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({
  server: https.createServer({
    key: fs.readFileSync(__dirname + '/ssl/key.pem'),
    cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
  }, function(req, res) {
    res.writeHead(200);
    res.end();
  }).listen(9999),
  path: "/ws"
});

wss.on('connection', function(socket) {
  var request = socket.upgradeReq;
  var response = {writeHead: {}};
  sessionHandler(request, response, function (err) {
    var email = request.session.user;
    storage.getUser(email).then(function(user) {
      if (!authenticate(user)) {
        console.log('auth failed');
        conn.end();
        return;
      }
      rtc(socket, storage, globalSession, user);
      ws(socket, storage, globalSession, user);
    });
  });

});



// listen
server.listen(3000);
