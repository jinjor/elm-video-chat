
module.exports = function(socket, storage, session) {

  socket.on('message', function(s) {
    //TODO refactor
    if(s === 'Ping') {
      return;
    }


    var data = JSON.parse(s);
    console.log(data.type);
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
      var client = room.getClient(data.to);
      client.send(JSON.stringify({
        type: 'answerCandidate',
        from: data.from,
        data: data.data
      }));
    }
  });

};
