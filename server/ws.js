
module.exports = function(socket, storage, session, user) {

  socket.on('message', function(s) {
    var data = JSON.parse(s);
    // console.log(data);
    if (data.type === 'join') {
      var room = session.getRoom(data.room);
      if (!room) {
        throw new Error('Room Not Found: ' + data.room);
      }
      session.users[data.from] = user;
      room.addClient(data.from, socket);
      room.getClients(data.from).forEach(function(client) {
        client.send(JSON.stringify({
          from: data.from,
          type: 'join',
          data: {
            user: session.users[data.from]
          }
        }));
      });
    } else if (data.type === 'message') {
      var roomId = data.room;
      var message = data.data.message;
      var time = data.data.time;
      var room = session.getRoom(roomId);

      room.addMessage(message);
      room.getClients().forEach(function(client) {
        client.send(JSON.stringify({
          from: data.from,
          type: 'message',
          data: {
            message: message,
            time: time
          }
        }));
      });
    } else if (data.type === 'endStream') {
      var roomId = data.room;
      var room = session.getRoom(roomId);
      room.getOtherClients(data.from).forEach(function(client) {
        client.send(JSON.stringify(data));
      });
    }
  });
  socket.on('close', function() {
    session.getRooms().forEach(function(room) {
      var removedId = room.removeClient(socket);
      // console.log('removed: ' + removedId)
      if (removedId) {
        room.getOtherClients(removedId).forEach(function(client) {
          client.send(JSON.stringify({
            from: removedId,
            type: 'leave',
            data: null
          }));
        });
      }
    });
  });

};
