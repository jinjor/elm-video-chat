

module.exports = function(app, staticRouter, storage, session, ws) {
  var iceServers =  [{
    url: 'stun:stun.l.google.com:19302'
  }/*, {
    url: 'stun:stun.anyfirewall.com:3478'
  }*/];
  app.get('/room/:id', function(req, res, next) {
    var roomId = req.params.id;
    session.getRoom(roomId) || session.createRoom(roomId);
    req.url = '/room.html';
    staticRouter(req, res, next);
  });
  app.get('/api/room/:id', function(req, res) {
    var roomId = req.params.id;
    var room = session.getRoom(roomId);
    if (room) {
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
    storage.getUser(req.session.user).then(function(user) {
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
