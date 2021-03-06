var uuid = require('uuid');
var Twit = require('twit');


module.exports = function(app, staticRouter, storage, session, ws) {

  var invite = session.isDevMode ? function invite(T, user, to, roomId, cb) {
    console.log('invite(debug): ' + to);
    console.log('invite(debug): ' + roomId);
    cb();
  } : function invite(T, user, to, roomId, cb) {
    var url = session.rootURL + '/room/' + roomId;// + '?via=twitter'
    T.post('direct_messages/new', {
        user_id : user.twitterId,
        screen_name: to,
        text: 'Invitation to Vity2! ' + url
      }, function(err, data, response) {
      // console.log(data)
      cb(err, data);
    });
  }

  function loadFollowers(T, user, cursor, cb) {
    //TODO followers && friends

    T.get('followers/list', {
      user_id : user.twitterId,
      skip_status: true,
      include_user_entities: false,
      cursor: cursor,
      count: 200
    }, function(err, data, response) {
      if(err) {
        cb(err, []);
        return;
      }
      // console.log(data.users[0]);

      //TODO
      var initial = {};
      initial[user.name + ' ' + user.displayName] = {
        name: user.displayName,
        screen_name: user.name,
        profile_image_url: user.image
      };

      user.followers = data.users.reduce(function(memo, _user) {
        var data = {
          id: _user.id,
          name: _user.name,
          screen_name: _user.screen_name,
          profile_image_url: _user.profile_image_url
        };
        memo[_user.screen_name + ' ' + _user.screen_name] = data;
        return memo;
      }, user.followers || initial);
      if(data.next_cursor) {
        loadFollowers(T, user, data.next_cursor, cb);
      } else {
        cb();
      }
    });

  }

  function searchFollowers(T, user, q, cb) {
    if(!user.followers) {
      cb(null, []);
    } else {
      f();
    }
    function f() {
      var regex = new RegExp('.*' + q + '.*');
      var followers = user.followers;
      var filtered = Object.keys(followers).filter(function(key) {
        return !!key.match(regex);
      }).map(function(key) {
        return followers[key];
      });
      cb(null, filtered);

      // T.get('users/search', {
      //   user_id : user.twitterId,
      //   q: q,
      //   count: 100,
      //   include_entities: false
      // }, function(err, data, response) {
      //   data = data.filter(function(user) {
      //     console.log(user)
      //     return followers[user.id_str];
      //   });
      //   console.log(followers, data);
      //   // TODO followers でフィルタリング
      //   cb(err, []);
      // });
    }

  }




  // var iceServers =  [{
  //   url: 'stun:stun.l.google.com:19302'
  // }, {
  //   url: 'turn:homeo@turn.bistri.com:80',
  //   credential: 'homeo'
  // }];
  // var iceServers =  [{
  //   url: 'stun:46.137.243.49:3478'
  // }];
  var iceServers =  [{
    url: 'stun:stun.l.google.com:19302'
  }];

  app.get('/room/:id', function(req, res, next) {
    //TODO
    if(!req.session.user) {
      req.session.redirectURL = req.url;
      res.redirect('/');
      return;
    }
    var roomId = req.params.id;
    session.getRoom(roomId) || session.createRoom(roomId);
    req.url = '/room.html';
    staticRouter(req, res, next);
  });
  app.post('/invite', function(req, res, next) {
    //TODO
    if(!req.session.user) {
      res.redirect('/');
      return;
    }
    storage.getUser(req.session.user).then(function(user) {
      if(user.authority !== 'twitter') {
        res.sendStatus(401);
        return;
      }
      var T = new Twit({
          consumer_key: req.session.passport.user.twitterConsumerKey
        , consumer_secret: req.session.passport.user.twitterConsumerSecret
        , access_token: req.session.passport.user.twitterAccessToken
        , access_token_secret: req.session.passport.user.twitterAccessTokenSecret
      });
      var roomId = uuid.v1();
      session.getRoom(roomId) || session.createPrivateRoom(roomId);
      var to = req.body.invited;

      invite(T, req.session.user, to, roomId, function(e) {
        if(e) {
          console.log(e);
          var errorMessage = 'error!';
          if(e.code === 150) {
            errorMessage = 'The user @' + to + ' does not exist or is not following you.';
          }
          res.send(errorMessage);
          return;
        }
        res.redirect('/room/' + roomId);
      });

    }).catch(function(e) {
      console.log(e);
      res.sendStatus(500);
    });

  });
  app.get('/api/room/:id', function(req, res) {
    var roomId = req.params.id;
    var room = session.getRoom(roomId);
    if (room) {
      //TODO
      // if(!req.session.user) {
      //   res.redirect('/');
      //   return;
      // }
      storage.getUser(req.session.user).then(function(user) {
        var users = {};
        var clientIds = room.getClientIds();
        // console.log(clientIds);
        clientIds.forEach(function(clientId) {
          users[clientId] = session.users[clientId];
        });
        var _room = {
          id: roomId,
          private: room.private,
          members: [],//TODO
          peers: clientIds,
          users: users
        };
        res.send({
          user: user,
          room: _room,
          iceServers: iceServers
        });
      }).catch(function(e) {
        console.log(e);
        res.sendStatus(500);
      });
    } else {
      res.sendStatus(404);
    }
  });
  app.get('/api/rooms', function(req, res) {

    //TODO
    if(!req.session.user) {
      res.redirect('/');
      return;
    }
    storage.getUser(req.session.user).then(function(user) {
      // console.log(user);
      var rooms = session.getRooms();
      var _rooms = rooms.map(function(room) {
        var users = {};
        var clientIds = room.getClientIds();
        clientIds.forEach(function(clientId) {
          users[clientId] = session.users[clientId];
        });
        return {
          id: room.id,
          private : room.private,
          members: [],//TODO
          peers: clientIds,
          users: users
        };
      });
      _rooms = _rooms.filter(function(room) {
        return !room.private;
      });
      if(req.session.passport.user) {// twitter acount
        if(!user.followers) {
          var T = new Twit({
              consumer_key: req.session.passport.user.twitterConsumerKey
            , consumer_secret: req.session.passport.user.twitterConsumerSecret
            , access_token: req.session.passport.user.twitterAccessToken
            , access_token_secret: req.session.passport.user.twitterAccessTokenSecret
          });
          loadFollowers(T, user, -1, function() {
          });
        }
      }
      res.send({
        rooms: _rooms,
        user: user,
        recentlyInvited: []//TODO
      });

    });

  });

  app.get('/api/search-user/:q', function(req, res) {
    //TODO
    if(!req.session.user) {
      res.redirect('/');
      return;
    }
    storage.getUser(req.session.user).then(function(user) {
      if(user.authority !== 'twitter') {
        res.sendStatus(401);
        return;
      }

      var T = new Twit({
          consumer_key: req.session.passport.user.twitterConsumerKey
        , consumer_secret: req.session.passport.user.twitterConsumerSecret
        , access_token: req.session.passport.user.twitterAccessToken
        , access_token_secret: req.session.passport.user.twitterAccessTokenSecret
      });
      var q = req.params.q;
      searchFollowers(T, user, q, function(e, data) {
        data = data.map(function(twitterUser) {
          return {
            name: twitterUser.screen_name,
            displayName: twitterUser.name,
            image: twitterUser.profile_image_url,
            authority: ""
          };
        });
        if(data.length > 10) {
          data.length = 10;
        }
        res.send(data);
      });
    });

  });

  app.post('/api/invite/:roomId', function(req, res) {
    //TODO
    if(!req.session.user) {
      res.redirect('/');
      return;
    }
    storage.getUser(req.session.user).then(function(user) {
      if(user.authority !== 'twitter') {
        res.sendStatus(401);
        return;
      }
      var T = new Twit({
          consumer_key: req.session.passport.user.twitterConsumerKey
        , consumer_secret: req.session.passport.user.twitterConsumerSecret
        , access_token: req.session.passport.user.twitterAccessToken
        , access_token_secret: req.session.passport.user.twitterAccessTokenSecret
      });
      var roomId = req.params.roomId;
      var to = req.body.invited;

      invite(T, req.session.user, to, roomId, function(e) {
        if(e) {
          console.log(e);
          var errorMessage = 'error!';
          if(e.code === 150) {
            errorMessage = 'The user @' + to + ' does not exist or is not following you.';
          }
          res.send(errorMessage);
          return;
        }
        res.end();
      });

    }).catch(function(e) {
      console.log(e);
      res.sendStatus(500);
    });

  });
};
