"use strict";

exports._create = function(callback) {
  return function() {
    return new ResizeObserver(function(entries, resizeObserver) {
      callback(entries)(resizeObserver)();
    });
  };
};

exports._observe = function(resizeObserver, el) {
  return function() {
    resizeObserver.observe(el);
  };
};

exports._disconnect = function(resizeObserver) {
  return function() {
    resizeObserver.disconnect();
  };
};

exports._unobserve = function(resizeObserver, el) {
  return function() {
    resizeObserver.unobserve(el);
  };
};

exports._borderBoxSize = function(entry) {
  return entry.borderBoxSize;
};

exports._contentBoxSize = function(entry) {
  return entry.contentBoxSize;
};

exports._contentRect = function(entry) {
  return entry.contentRect;
};

exports._target = function(entry) {
  return entry.target;
};
