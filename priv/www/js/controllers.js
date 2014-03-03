var configApp = angular.module('configApp', [
    'ngRoute',
    'ngResource',
    'configControllers']);

/***************************************************************************
 * Route Config
 ***************************************************************************/

configApp.config(['$routeProvider',
		  function($routeProvider) {
		      $routeProvider.
			  when('/base', {
			      templateUrl: 'partials/base.html',
			      controller: 'BaseCtrl'
			  }).
			  when('/mqtt_broker', {
			      templateUrl: 'partials/mqtt_broker.html',
			      controller: 'MqttBrokerCtrl'
			  }).
			  when('/subscribes', {
			      templateUrl: 'partials/subscribes.html',
			      controller: 'SubscribesCtrl'
			  }).
			  when('/gpio', {
			      templateUrl: 'partials/gpio.html',
			      controller: 'GpioCtrl'
			  }).
			  when('/arduino', {
			      templateUrl: 'partials/arduino.html',
			      controller: 'ArduinoCtrl'
			  }).
			  when('/omron_plc', {
			      templateUrl: 'partials/omron_plc.html',
			      controller: 'OmronFinsCtrl'
			  }).
			  otherwise({
			      redirectTo: '/base'
			  });
		  }]);

/***************************************************************************
 * Controllers
 ***************************************************************************/

var configControllers = angular.module('configControllers', []);

// navigation var controller
configControllers.controller('NavCtrl', function($scope, $location) {
    $scope.selected = undefined;
    $scope.menuList = [
	{title: "Base"      , href:"#/base"       },
	{title: "MQTT"      , href:"#/mqtt_broker"},
	{title: "Subscribe" , href:"#/subscribes" },
	{title: "GPIO"      , href:"#/gpio"       },
	{title: "Arduino"   , href:"#/arduino"    },
	{title: "OMRON PLC" , href:"#/omron_plc"  }
    ];
    
    $scope.setActionButton = function(path) {
	$scope.menuList.forEach(function(m) {
	    if (m.href === path) {
		$scope.selected = m;
	    }
	})
    }

    $scope.setActionButton("#" + $location.path());
});

// base config
configControllers.controller('BaseCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.base = undefined;

    var Base = $resource('/api/config/base.json', {});
    var base = Base.get(function() {
	$scope.base = base;
	setWatch('base', $scope);
    });
    
    $scope.save = function() {
	base.$save(function() {
	    $scope.changed = false;
	    setWatch('base', $scope);
	});
    }
});

// mqtt broker config
configControllers.controller('MqttBrokerCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.mqtt = undefined;

    var Mqtt = $resource('/api/config/mqtt_broker.json', {});
    var mqtt = Mqtt.get(function() {
	$scope.mqtt = mqtt;
	setWatch('mqtt', $scope);
    });

    $scope.save = function() {
	mqtt.$save(function(){ 
	    $scope.changed = false; 
	    setWatch('mqtt', $scope);
	});
    }
});

// subscribes config
configControllers.controller('SubscribesCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.subscribes = undefined;
    $scope.isChangedFlag = [];
    $scope.qosList = [0, 1, 2];

    var Subscribe = $resource('/api/config/subscribes.json', {});
    var subscribes = Subscribe.get(function() {
	$scope.subscribes = subscribes;
	subscribes.subscribes.forEach(function(e) {
	    $scope.isChangedFlag.push(false);
	})
	setWatch('subscribes', $scope);
    });

    $scope.addSubscriber = function() {
	subscribes.subscribes.push({topic:"", qos:0});
    }

    $scope.delSubscriber = function(index) {
	var newArray = [];

	for(var i=0; i < subscribes.subscribes.length; i++) {
	    if (i != index) { newArray.push(subscribes.subscribes[i]); }
	}

	subscribes.subscribes = newArray;
    }

    $scope.save = function() {
	subscribes.$save(function() {
	    $scope.changed = false;
	    setWatch('subscribes', $scope);
	});
    }

});

// GPIO config
configControllers.controller('GpioCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.pin_list = []; for(var i=1; i<50; i++){ $scope.pin_list.push(i); }
    $scope.mode_list = ["in", "out"];
    $scope.edge_list = ["rising", "falling", "both", "none"];
    $scope.pull_list = ["up", "down", "none"];

    $scope.gpio = undefined;
 
    var Gpio = $resource('/api/config/gpio.json', {});
    var gpio = Gpio.get(function() {
	$scope.gpio = gpio;
	setWatch('gpio', $scope);
    });

    $scope.save = function() {
	gpio.$save(function() {
	    $scope.changed = false;
	    setWatch('gpio', $scope);
	})
    }

});

// Arduino config
configControllers.controller('ArduinoCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.analogList = []; for(var i=0; i<5; i++){ $scope.analogList.push(i); }
    $scope.analogUsingState = [false, false, false, false, false, false];
    $scope.mode_list = ["in", "out", "servo", "pwm"];
    $scope.pull_list = ["up", "none"];
    $scope.detailStyle = { display:'none' };
    $scope.arduino = undefined;
 
    var Arduino = $resource('/api/config/arduino.json', {});
    var arduino = Arduino.get(function() {
	$scope.arduino = arduino;
	$scope.enableChanged();
	arduino.analog.forEach(function(aiNo) {
	    $scope.analogUsingState[aiNo] = true;
	});
	setWatch('arduino', $scope);
    });

    $scope.save = function() {
	arduino.$save(function() {
	    $scope.changed = false;
	    setWatch('arduino', $scope);
	})
    }

    $scope.analogSelectChanged = function() {
	var newAnalogs = [];

	for(var aiNo = 0; aiNo < $scope.analogUsingState.length; aiNo++) {
	    if ($scope.analogUsingState[aiNo]) { newAnalogs.push(aiNo); }
	}

	$scope.arduino.analog = newAnalogs;
    }

    $scope.toInteger = function(name) {
	arduino[name] = Number(arduino[name]);
    }

    $scope.enableChanged = function() {
	if ($scope.arduino.arduino_enable) {
	    $scope.detailStyle = { display:'block' };
	} else {
	    $scope.detailStyle = { display:'none' };
	}
    }

});

// omron fins
configControllers.controller('OmronFinsCtrl', function($scope, $resource) {
    $scope.changed = false;
    $scope.detailStyle = { display:'none' };
    $scope.fins = undefined;

    $scope.analogInput = "0"

    var OmronFins = $resource('/api/config/omron_fins.json', {});
    var fins = OmronFins.get(function() {
	$scope.fins = fins;
	$scope.enableChanged();
	setDefaultAnalogInput();
	setDefaultDigitalInput();
	setWatch('fins', $scope);
    });
    
    $scope.save = function() {
	fins.$save(function() {
	    $scope.changed = false;
	    setWatch('fins', $scope);
	});
    }

    $scope.toInteger = function(name) {
	fins.omron_fins[name] = Number(fins.omron_fins[name]);
    }

    $scope.enableChanged = function() {
	if ($scope.fins.omron_fins_enable) {
	    $scope.detailStyle = { display:'block' };
	} else {
	    $scope.detailStyle = { display:'none' };
	}
    }

    $scope.addAnalog = function() {
	fins.omron_fins.analog.push(parseInt($scope.analogInput));
	fins.omron_fins.analog = sortInteger(fins.omron_fins.analog);
	setDefaultAnalogInput();
    }

    $scope.delAnalog = function() {
	var array = [];
	var value = parseInt($scope.analogInput);

	fins.omron_fins.analog.forEach(function(e) {
	    if (e != value) array.push(e);
	})

	fins.omron_fins.analog = sortInteger(array);
	setDefaultAnalogInput();
    }

    $scope.addDigital = function() {
	fins.omron_fins.digital.push(parseInt($scope.digitalInput));
	fins.omron_fins.digital = sortInteger(fins.omron_fins.digital);
	setDefaultDigitalInput();
    }

    $scope.delDigital = function() {
	var array = [];
	var value = parseInt($scope.digitalInput);

	fins.omron_fins.digital.forEach(function(e) {
	    if (e != value) array.push(e);
	})

	fins.omron_fins.digital = sortInteger(array);
	setDefaultDigitalInput();
    }

    setDefaultAnalogInput = function() {
	if (fins.omron_fins.analog.length === 0) {
	    $scope.analogInput = ""
	} else {
	    $scope.analogInput = Math.max.apply(null, 
						fins.omron_fins.analog) + 1;
	}
    }

    setDefaultDigitalInput = function() {
	if (fins.omron_fins.digital.length === 0) {
	    $scope.digitalInput = ""
	} else {
	    $scope.digitalInput = Math.max.apply(null, 
						fins.omron_fins.digital) + 1;
	}
    }

    sortInteger = function(array) {
	array.sort(function(a1, a2) {
	    if (a1 < a2) return -1;
	    if (a1 > a2) return  1;
	    return 0;
	});
	return array
    }

});

var setWatch = function(target, scope) {
    setTimeout(function() {
	scope.changed = false;
	scope.$watch(target, function() { scope.changed = true; });
    }, 300);
}
