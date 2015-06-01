

module.exports = function(app, staticRouter, storage, session, ws) {
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
        clientIds.forEach(function(clientId) {
          users[clientId] = session.users[clientId];
        });
        res.send({
          user: user,
          room: {
            id: roomId,
            peers: clientIds,
            users: users
          }
        });
      });
    } else {
      res.status(404).end();
    }
  });
  app.get('/api/rooms', function(req, res) {
    var rooms = session.getRooms();
    res.send(rooms.map(function(room) {
      return {
        id: room.id,
        peers: room.getClientIds().map(function(clientId) {
          return {
            id: clientId,
            name: clientId.substring(0, 7),
            mail: 'foo@bar'
          };
        })
      };
    }));
  });


};
