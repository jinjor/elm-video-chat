function Room(id) {
  this.id = id;
  this.clients = {};
  this.messages = [];
}
Room.prototype.addClient = function(id, socket) {
  this.clients[id] = socket;
};
Room.prototype.getClient = function(id) {
  return this.clients[id];
};
Room.prototype.removeClient = function(socket) {
  var exists = false;
  this.getClientIds().forEach(function(id) {
    if(this.clients[id] === socket) {
      delete this.clients[id];
      exists = true;
    }
  }.bind(this));
  return exists;
};
Room.prototype.getClients = function() {
  return this.getClientIds().map(function(id) {
    return this.clients[id];
  }.bind(this));
};
Room.prototype.getOtherClients = function(selfSocketId) {
  return this.getClientIds().filter(function(id) {
    return id !== selfSocketId;
  }).map(function(id) {
    return this.clients[id];
  }.bind(this));
};


Room.prototype.getClientIds = function() {
  return Object.keys(this.clients);
};
Room.prototype.getMessages = function() {
  return this.messages;
};
Room.prototype.addMessage = function(message) {
  return this.messages.push(message);
};
var rooms = {};


var getRoom = function(roomId) {
  return rooms[roomId]
};
var getRooms = function() {
  return Object.keys(rooms).map(function(roomId) {
    return getRoom(roomId);
  });
};
var createRoom = function(roomId) {
  rooms[roomId] = new Room(roomId);
};

module.exports = {
  getRoom: getRoom,
  getRooms: getRooms,
  createRoom: createRoom
};
