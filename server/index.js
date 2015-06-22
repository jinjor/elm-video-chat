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
var twitter = require('twitter');
var oauth = require('oauth');
var util = require('util');

var port = process.env.PORT || 3000;

// var passport = require('passport');
// var TwitterStrategy = require('passport-twitter').Strategy;
//
// // Passport: TwitterのOAuth設定
// passport.use(new TwitterStrategy({
//     consumerKey: "your key",
//     consumerSecret: "your secret",
//     callbackURL: "/"
//   }, function(token, tokenSecret, profile, done) {
//   // ユーザIDを設定
//   profile.uid = profile.provider + profile.id;
//   process.nextTick(function() {
//     return done(null, profile);
//   });
// }));
//
//   // Serialized and deserialized methods when got from session
// passport.serializeUser(function(user, done) {
//   done(null, user);
// });
//
// passport.deserializeUser(function(user, done) {
//   done(null, user);
// });

  // Define a middleware function to be used for every secured routes
  // 認証が通っていないところは401を返す
var auth = function(req, res, next) {
  if (!req.isAuthenticated()) {
    res.send(401);
  } else {
    next();
  }
};


//認証
var api = new twitter({
    consumer_key: '', //ここに取得したコンシューマーキーを入れる(以下3行も同様にそれぞれ入れる)
    consumer_secret: '',
    access_token_key: '',
    access_token_secret: ''
});

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

// app.use(passport.initialize()); // Add passport initialization
// app.use(passport.session());    // Add passport initialization

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


// OAuth
var _twitterConsumerKey = "twitterConsumerKey";
var _twitterConsumerSecret = "twitterConsumerSecret";
var oauth2 = new oauth.OAuth2(_twitterConsumerKey,
       _twitterConsumerSecret,
       'https://api.twitter.com/',
       null,
       'oauth2/token',
       null);
// var consumer = new oauth.OAuth(
//     "https://twitter.com/oauth/request_token", "https://twitter.com/oauth/access_token",
//     _twitterConsumerKey, _twitterConsumerSecret, "1.0A", "http://127.0.0.1:8080/sessions/callback", "HMAC-SHA1");


app.get('/oauth', function(req, res) {
  oauth2.getOAuthAccessToken(
      '',
      {'grant_type':'client_credentials', 'redirect_uri': '/'},
      function (e, access_token, refresh_token, results){
        if (e) {
             console.log(e);
             res.end("error!");
         } else if (results.error) {
             console.log(results);
             res.end(JSON.stringify(results));
         }
         else {
             console.log('Obtained access_token: ', access_token);
             res.end( access_token);
         }
    });
});

// app.get('/sessions/connect', function(req, res) {
//
//   consumer.getOAuthRequestToken(function(error, oauthToken, oauthTokenSecret, results){
//     if (error) {
//       res.send("Error getting OAuth request token : " + util.inspect(error), 500);
//     } else {
//       req.session.oauthRequestToken = oauthToken;
//       req.session.oauthRequestTokenSecret = oauthTokenSecret;
//       res.redirect("https://twitter.com/oauth/authorize?oauth_token="+req.session.oauthRequestToken);
//     }
//   });
// });
// app.get('/sessions/callback', function(req, res){
//   util.puts(">>"+req.session.oauthRequestToken);
//   util.puts(">>"+req.session.oauthRequestTokenSecret);
//   util.puts(">>"+req.query.oauth_verifier);
//   consumer.getOAuthAccessToken(
//     req.session.oauthRequestToken,
//     req.session.oauthRequestTokenSecret,
//     req.query.oauth_verifier,
//     function(error, oauthAccessToken, oauthAccessTokenSecret, results) {
//     if (error) {
//       res.send("Error getting OAuth access token : " +
//         util.inspect(error) +
//         "[" + oauthAccessToken + "]" +
//         "[" + oauthAccessTokenSecret + "]" +
//         "[" + util.inspect(results) + "]", 500);
//     } else {
//       req.session.oauthAccessToken = oauthAccessToken;
//       req.session.oauthAccessTokenSecret = oauthAccessTokenSecret;
//
//       res.redirect('/home');
//     }
//   });
// });
// app.get('/home', function(req, res){
//     consumer.get("http://twitter.com/account/verify_credentials.json", req.session.oauthAccessToken, req.session.oauthAccessTokenSecret, function (error, data, response) {
//       if (error) {
//           res.redirect('/sessions/connect');
//           // res.send("Error getting twitter screen name : " + util.inspect(error), 500);
//       } else {
//           var parsedData = JSON.parse(data);
//
//         // req.session.twitterScreenName = response.screen_name;
//         res.send('You are signed in: ' + parsedData.screen_name);
//       }
//     });
// });

// setup REST


rest(app, staticRouter, storage, globalSession);

var server = https.createServer({
  key: fs.readFileSync(__dirname + '/ssl/key.pem'),
  cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
}, app);

// setup websocket
// var WebSocketServer = require('ws').Server;
// var wss = new WebSocketServer({
//   server: https.createServer({
//     key: fs.readFileSync(__dirname + '/ssl/key.pem'),
//     cert: fs.readFileSync(__dirname + '/ssl/cert.pem')
//   }, function(req, res) {
//     res.writeHead(200);
//     res.end();
//   }).listen(9999),
//   path: "/ws"
// });
//
// wss.on('connection', function(socket) {
//   var request = socket.upgradeReq;
//   var response = {writeHead: {}};
//   sessionHandler(request, response, function (err) {
//     var email = request.session.user;
//     storage.getUser(email).then(function(user) {
//       if (!authenticate(user)) {
//         console.log('auth failed');
//         socket.close();
//         return;
//       }
//       rtc(socket, storage, globalSession, user);
//       ws(socket, storage, globalSession, user);
//     });
//   });
//
// });

// listen
server.listen(port);
console.log("Server listening on port: " + port);
