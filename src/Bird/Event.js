// module Bird.Event

exports._listen = function (unit) {
    return function (id) {
        return function (event) {
            return function (handler) {
                return function () {
                    var elem = document.getElementById(id);
                    if (elem) {
                        elem.addEventListener(event, function (_) {
                            handler();
                        });
                    };
                    return unit;
                };
            };
        };
    };
};

exports._keydown = function (unit) {
    return function (key) {
        return function (handler) {
            return function () {
                window.addEventListener("keydown", function (event) {
                    if (key === event.key) {
                        handler();
                    };
                });
                return unit;
            };
        };
    };
};

exports._keyup = function (unit) {
    return function (key) {
        return function (handler) {
            return function () {
                window.addEventListener("keyup", function (event) {
                    if (key === event.key) {
                        handler();
                    };
                });
                return unit;
            };
        };
    };
};

exports._key = function (unit) {
    return function (key) {
        return function (handler) {
            return function () {
                var timer = null;
                window.addEventListener("keydown", function (event) {
                    if (key === event.key && !timer) {
                        timer = setInterval(handler, 1000/60);
                    };
                });
                window.addEventListener("keyup", function (event) {
                    if (key === event.key && timer) {
                        clearInterval(timer);
                        timer = null;
                    };
                });
                return unit;
            };
        };
    };
};

exports._mousedown = function (unit) {
    return function (id) {
        return function (handler) {
            return function () {
                var elem = document.getElementById(id);
                if (elem) {
                    elem.addEventListener("mousedown", function (event) {
                        handler(event.button)(event.clientX)(event.clientY)();
                    });
                };
                return unit;
            };
        };
    };
};

exports._frames = function (unit) {
    return function (handler) {
        return function () {
            var last = Date.now();
            window.requestAnimationFrame(function tick(_) {
                var now = Date.now();
                handler(now - last)();
                last = now;
                window.requestAnimationFrame(tick);
            });
            return unit;
        };
    };
};

exports._after = function (unit) {
    return function (delay) {
        return function (handler) {
            return function () {
                setTimeout(handler, delay);
                return unit;
            };
        };
    };
};

exports._resize = function (unit) {
    return function (handler) {
        return function () {
            window.addEventListener("resize", function(_) {
                handler();
            });
            return unit;
        };
    };
};

exports._setCanvasBackground = function (unit) {
    return function(bg) {
        return function (width) {
            return function(height) {
                return function () {
                    var elem = document.getElementById("canvas");
                    var widthpx = width.toString() + "px";
                    var heightpx = height.toString() + "px";
                    if (elem) {
                        elem.style.backgroundImage = bg;
                        elem.style.backgroundSize = widthpx + " " + heightpx;
                        elem.style.width = widthpx;
                        elem.style.height = heightpx;
                    };
                    return unit;
                };
            };
        };
    };
};


exports._music = function (unit) {
    return function (song) {
        var audio = new Audio(song);
        audio.loop = true;
        return function () {
            audio.play();
        };
    };
};

exports._sound = function (unit) {
    return function (song) {
        var audio = new Audio(song);
        return function () {
            audio.play();
        };
    };
};
