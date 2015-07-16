var http = require('http');
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
var util = require('util');
var passport = require('passport');
var TwitterStrategy = require('passport-twitter').Strategy;
var Twit = require('twit');

var port = process.env.PORT || 3000;
var needsHttps = !process.env.PORT;
var _twitterConsumerKey = process.env.TWITTER_CONSUMER_KEY;
var _twitterConsumerSecret = process.env.TWITTER_CONSUMER_SECRET;
var isHeroku = !!process.env.PORT;
var isDevMode = !isHeroku;

passport.use(new TwitterStrategy({
    consumerKey: _twitterConsumerKey,
    consumerSecret: _twitterConsumerSecret,
    callbackURL: "/oauth/twitter/callback"
  }, function(token, tokenSecret, profile, done) {
    profile.twitterConsumerKey = _twitterConsumerKey;
    profile.twitterConsumerSecret = _twitterConsumerSecret;
    profile.twitterAccessToken = token;
    profile.twitterAccessTokenSecret = tokenSecret;
    process.nextTick(function() {
      return done(null, profile);
    });
}));
passport.serializeUser(function(user, done) {
  done(null, user);
});
passport.deserializeUser(function(user, done) {
  done(null, user);
});


var authenticate = function(user) {
  return true;
};

var app = express();
var sessionHandler = session({
  name: 'elm-video-chat',
  secret: 'secret',
  // cookie: { secure: true }
});
app.use(sessionHandler);

app.use(passport.initialize());
app.use(passport.session());
// force https
if(isHeroku) {
  app.get('*', function(req, res, next) {
    if(req.headers['x-forwarded-proto'] != 'https') {
      res.redirect('https://vity2.herokuapp.com' + req.url);
    } else {
      next();
    }
  });
}

app.get('/oauth/twitter', passport.authenticate('twitter', {forceLogin: true}));
app.get('/oauth/twitter/callback',
  passport.authenticate('twitter', {
    successRedirect: '/',
    failureRedirect: '/?oauth=fail'
  }));
app.use(bodyParser());

var loginCheck = function(req, res, next) {
  // console.log("req.session.user:" + req.session.user)

  if (req.method === 'POST' && req.url === '/api/login') {
    var name = req.body.name;
    req.session.user = name;
    res.redirect(req.header('referrer'));
  } else if (req.method === 'GET' && req.url === '/logout') {

    // req.logout();
    req.session.destroy();
    console.log('deleted session');
    res.redirect('/');
  } else {
    if (req.isAuthenticated()) {
      // console.log(req.session.passport.user);
      //id, username, displayName, photos:[{value:string}], provider, _json
      // profile_image_url
      var _user = req.session.passport.user;
      var uid = _user.provider + _user.id;
      var user = {
        id: uid,
        email: 'foo@bar',
        mail: 'foo@bar',
        provider: _user.provider,
        twitterId: _user.id,
        name: _user.username,
        displayName: _user.displayName,
        image: _user._json.profile_image_url,
        authority: "twitter"
      };
      storage.addUser(uid, user);
      req.session.user = uid;
      if (req.session.redirectURL) {
        var url = req.session.redirectURL;
        req.session.redirectURL = null;
        res.redirect(url);
      } else {
        next();
      }
    } else if(req.session.user) {//TODO guest for debug
      storage.addUser(req.session.user, {
        id: req.session.user,
        name:req.session.user,
        displayName:req.session.user,
        image: "/default_profile.png",
        authority: "guest"
      });
      if (req.session.redirectURL) {
        var url = req.session.redirectURL;
        req.session.redirectURL = null;
        res.redirect(url);
      } else {
        next();
      }
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

globalSession.rootURL = isHeroku ? 'https://vity2.herokuapp.com' : 'https://localhost:' + port;//TODO
globalSession.isDevMode = isDevMode;

rest(app, staticRouter, storage, globalSession);

var server = https.createServer({
  key: fs.readFileSync(__dirname + '/ssl/key.pem'),
  cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
}, app);

var server = needsHttps ?
  https.createServer({
    key: fs.readFileSync(__dirname + '/ssl/key.pem'),
    cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
  }, app) :
  http.createServer(app);

// setup websocket
var WebSocketServer = require('ws').Server;
var wss = new WebSocketServer({
  // server: https.createServer({
  //   key: fs.readFileSync(__dirname + '/ssl/key.pem'),
  //   cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
  // }, function(req, res) {
  //   res.writeHead(200);
  //   res.end();
  // }).listen(9999),
  server: server,
  path: "/ws"
});

wss.on('connection', function(socket) {
  socket.on('message', function(s) {
    if(s === 'Ping') {
      // console.log('ping');
      socket.send('Pong');
    }
  });

  var request = socket.upgradeReq;
  var response = {writeHead: {}};
  sessionHandler(request, response, function (err) {
    var email = request.session.user;
    storage.getUser(email).then(function(user) {
      if (!authenticate(user)) {
        console.log('auth failed');
        socket.close();
        return;
      }
      rtc(socket, storage, globalSession, user);
      ws(socket, storage, globalSession, user);
    });
  });
});



// listen
server.listen(port);
console.log("Server listening on port: " + port);
