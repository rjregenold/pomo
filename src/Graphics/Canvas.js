"use strict";

exports._setTextBaseline = function(ctx, baseline) {
  return function() {
    ctx.textBaseline = baseline;
  };
};

exports._getTextHeight = function(fontFamily, fontSize, text) {
  return function() {
    const div = document.createElement('div');
    div.innerHTML = text;
    div.style.position = 'absolute';
    div.style.top = '-10000px';
    div.style.left = '-10000px';
    div.style.fontFamily = fontFamily;
    div.style.fontSize = fontSize;
		div.style.lineHeight = 'normal';
    document.body.appendChild(div);
    const textHeight = div.offsetHeight;
    document.body.removeChild(div);
    return textHeight;
  };
};
