"use strict";

exports._setTextBaseline = function(ctx, baseline) {
  return function() {
    ctx.textBaseline = baseline;
  };
};
