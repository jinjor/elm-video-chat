
module.exports = function(socket, storage, session, user) {

  socket.on('message', function(s) {
    var data = JSON.parse(s);
    if (data.type === 'join') {
      var room = session.getRoom(data.room);
      if (!room) {
        throw new Error('Room Not Found: ' + data.room);
      }
      session.users[data.from] = user;
      room.addClient(data.from, socket);
      room.getClients().forEach(function(client) {
        client.send(JSON.stringify({
          type: 'update'
        }));
      });
    } else if (data.type === 'message') {
      var roomId = data.room;
      var message = data.message;
      var room = session.getRoom(roomId);
      //TODO getUserFromCookie
      var message = {
        userId: '',
        message: message
      };

      room.addMessage(message);
      room.getClients().forEach(function(client) {
        client.send(JSON.stringify({
          type: 'message',
          message: message
        }));
      });

    }

  });
  socket.on('close', function() {
    session.getRooms().forEach(function(room) {
      var removed = room.removeClient(socket);
      if (removed) {
        room.getClients().forEach(function(client) {
          client.send(JSON.stringify({
            type: 'update'
          }));
        });
      }
    });
  });

};
