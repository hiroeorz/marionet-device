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
	{title: "Base"     , href:"#/base"       },
	{title: "MQTT"     , href:"#/mqtt_broker"},
	{title: "Subscribe", href:"#/subscribes" },
	{title: "GPIO"     , href:"#/gpio"       },
	{title: "Arduino  ", href:"#/arduino"    }
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
    $scope.arduino = undefined;
 
    var Arduino = $resource('/api/config/arduino.json', {});
    var arduino = Arduino.get(function() {
	$scope.arduino = arduino;
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
});

var setWatch = function(target, scope) {
    setTimeout(function() {
	scope.$watch(target, function() { scope.changed = true; });
    }, 1000);
}
