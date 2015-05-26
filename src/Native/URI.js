
Elm.Native.URI = {};

Elm.Native.URI.make = function(elm) {
    elm.Native = elm.Native || {};
    elm.Native.URI = elm.Native.URI || {};
    if (elm.Native.URI.values) return elm.Native.URI.values;

    return elm.Native.URI.values = {
      encodeURI: encodeURI,
      decodeURI: decodeURI
    };
};
