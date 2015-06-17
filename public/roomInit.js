function uuid() {
  var uuid = "", i, random;
  for (i = 0; i < 32; i++) {
    random = Math.random() * 16 | 0;
    if (i == 8 || i == 12 || i == 16 || i == 20) {
      uuid += "-"
    }
    uuid += (i == 12 ? 4 : (i == 16 ? (random & 3 | 8) : random)).toString(16);
  }
  return uuid;
}
var clientId = window.clientId || uuid();//TODO

(function() {

  function getRoom() {
    var roomId = decodeURI(location.href.split('/room/')[1].split('?')[0]);
    return roomId;
  }

  //--------------

  var roomSignal = Elm.fullscreen(Elm.Main, {
    clientId: clientId,
    roomName: getRoom(),
    updateRoom: getRoom(),
    websocketRunner: []
  });


})();
