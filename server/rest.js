var uuid = require('uuid');
var Twit = require('twit');


module.exports = function(app, staticRouter, storage, session, ws) {
  function invite(T, user, to, roomId, cb) {
    T.post('direct_messages/new', {
        user_id : user.twitterId,
        screen_name: to,
        text: 'Invitation to Vity2! ' + session.rootURL + '/room/' + roomId
      }, function(err, data, response) {
      // console.log(data)
      cb(err, data);
    });
  }

  var iceServers =  [{
    url: 'stun:stun.l.google.com:19302'
  }, {
    url: 'turn:homeo@turn.bistri.com:80',
    credential: 'homeo'
  }];
  app.get('/room/:id', function(req, res, next) {
    //TODO
    if(!req.session.user) {
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
    console.log(req.session.passport.user);
    var T = new Twit({
        consumer_key: req.session.passport.user.twitterConsumerKey
      , consumer_secret: req.session.passport.user.twitterConsumerSecret
      , access_token: req.session.passport.user.twitterAccessToken
      , access_token_secret: req.session.passport.user.twitterAccessTokenSecret
    });
    var roomId = uuid.v1();
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
        res.send({
          user: user,
          room: {
            id: roomId,
            peers: clientIds,
            users: users
          },
          iceServers: iceServers
        });
      });
    } else {
      res.status(404).end();
    }
  });
  app.get('/api/rooms', function(req, res) {
    //TODO
    if(!req.session.user) {
      res.redirect('/');
      return;
    }
    storage.getUser(req.session.user).then(function(user) {
      console.log(user);

      var rooms = session.getRooms();
      var _rooms = rooms.map(function(room) {
        var users = {};
        var clientIds = room.getClientIds();
        clientIds.forEach(function(clientId) {
          users[clientId] = session.users[clientId];
        });
        return {
          id: room.id,
          peers: clientIds,
          users: users
        };
      });
      res.send({
        rooms: _rooms,
        user: user
      });
    });

  });


};
