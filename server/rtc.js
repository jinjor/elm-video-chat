
module.exports = function(socket, storage, session) {

  socket.on('message', function(s) {
    var data = JSON.parse(s);
    console.log(data);
    if (data.type === 'offerSDP') {
      var roomId = data.room;
      var room = session.getRoom(roomId);
      var client = room.getClient(data.to);
      client.send(JSON.stringify({
        type: 'offerSDP',
        from: data.from,
        data: data.data
      }));
    } else if (data.type === 'answerSDP') {
      var roomId = data.room;
      var room = session.getRoom(roomId);
      var client = room.getClient(data.to);
      client.send(JSON.stringify({
        type: 'answerSDP',
        from: data.from,
        data: data.data
      }));
    } else if (data.type === 'offerCandidate') {
      var roomId = data.room;
      var room = session.getRoom(roomId);
      var client = room.getClient(data.to);
      client.send(JSON.stringify({
        type: 'offerCandidate',
        from: data.from,
        data: data.data
      }));
    } else if (data.type === 'answerCandidate') {
      var roomId = data.room;
      var room = session.getRoom(roomId);
      console.log(data.to);
      console.log(room.getClientIds());
      var client = room.getClient(data.to);
      client.send(JSON.stringify({
        type: 'answerCandidate',
        from: data.from,
        data: data.data
      }));
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
