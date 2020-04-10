"use strict";

function checkNotificationPromise() {
  try {
    Notification.requestPermission().then();
  } catch (e) {
    return false;
  }

  return true;
}

exports._requestPermission = function(toPermission) {
  return function(onError, onSuccess) {
    function handlePermission(permissionStr) {
      // hack to make chrome store the permission choice
      if (!('permission' in Notification)) {
        Notification.permission = permissionStr
      }

      onSuccess(toPermission(permissionStr));
    }

    if (!('Notification' in window)) {
      onError(new Error('This browser does not support notifications.'));
    } else {
      if (checkNotificationPromise()) {
        Notification.requestPermission().then(handlePermission);
      } else {
        Notification.requestPermission(handlePermission);
      }
    }

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};

exports._createNotification = function(title, body) {
  return new Notification(title, { body });
};
