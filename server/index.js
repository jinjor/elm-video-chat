var http = require('http');
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
app.use(session({
  name: 'elm-video-chat',
  secret: 'ssshhhhh',
  cookie: {
    secure: false,
    httpOnly: false
  }
}));
app.use(bodyParser());

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
      fs.createReadStream('public/login.html').pipe(res);
    }
  }
};
app.use(loginCheck);

// setup static
var staticRouter = express.static(__dirname + '/../public');
app.use(staticRouter);

// setup REST
rest(app, staticRouter, storage, globalSession);

// setup websocket
var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({
  port: 9999
});

wss.on('connection', function(socket) {
  // console.log(socket.upgradeReq.headers.cookie);
  var socketId = uuid.v1();
  var user = {};
  if (!authenticate(user)) {
    conn.end();
    return;
  }
  rtc(socket, storage, globalSession);
  ws(socket, storage, globalSession);
});



// listen for each
app.listen(3000);
