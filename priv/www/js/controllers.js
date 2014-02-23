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

// base config
configControllers.controller('BaseCtrl', function($scope, $resource) {
    $scope.deviceId = "";
    $scope.groupId = "";

    var Base = $resource('/api/config/base.json', {});
    var base = Base.get(function() {
	syncJsonData(base);
    });
    
    var syncJsonData = function(obj) {
	$scope.deviceId = obj.device_id;
	$scope.groupId = obj.group_id;
    }

    $scope.save = function() {
	base.$save();
    }
});

// mqtt broker config
configControllers.controller('MqttBrokerCtrl', function($scope, $resource) {
    $scope.mqtt = undefined;

    var Mqtt = $resource('/api/config/mqtt_broker.json', {});
    var mqtt = Mqtt.get(function() {
	$scope.mqtt = mqtt;
    });

    $scope.save = function() {
	mqtt.$save();
    }
});

// subscribes config
configControllers.controller('SubscribesCtrl', function($scope, $resource) {
    $scope.subscribes = undefined;
    $scope.isChangedFlag = [];
    $scope.qosList = [0, 1, 2];

    var Subscribe = $resource('/api/config/subscribes.json', {});

    var subscribes = Subscribe.get(function() {
	$scope.subscribes = subscribes;
	subscribes.subscribes.forEach(function(e) {
	    $scope.isChangedFlag.push(false);
	})
    });

    $scope.addSubscriber = function() {
	$scope.subscribes.push({topic:"", qos:0});
    }

    $scope.save = function() {
	subscribes.$save();
    }

});

// GPIO config
configControllers.controller('GpioCtrl', function($scope, $resource) {
    $scope.pin_list = []; for(var i=1; i<50; i++){ $scope.pin_list.push(i); }
    $scope.mode_list = ["in", "out"];
    $scope.edge_list = ["rising", "falling", "both", "none"];
    $scope.pull_list = ["up", "down", "none"];

    $scope.gpio = undefined;
 
    var Gpio = $resource('/api/config/gpio.json', {});
    var gpio = Gpio.get(function() { $scope.gpio = gpio; });

    $scope.save = function() {
	gpio.$save()
    }

});

// Arduino config
configControllers.controller('ArduinoCtrl', function($scope, $resource) {
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
	})
    });

    $scope.save = function() {
	arduino.$save()
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
//	arduino.sampling_interval = Number(arduino.sampling_interval);
    }
});
