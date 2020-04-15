"use strict";

exports._create = function(url) {
  return function() {
    return new Audio(url);
  };
};

exports._play = function(audio) {
  return function() {
    audio.play();
  };
};

exports._setVolume = function(audio, vol) {
  return function() {
    if (audio) audio.volume = vol;
  };
};
