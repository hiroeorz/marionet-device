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
	console.log("save");
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
	console.log("save");
	mqtt.$save();
    }
});

// subscribes config
configControllers.controller('SubscribesCtrl', function($scope, $resource) {
    $scope.topics = [];
    $scope.qoss = [];

    var Subscribe = $resource('/api/config/subscribes.json', {});
    var subscribe = Subscribe.get(function() {
	syncJsonData(subscribe);
    });
		     
    var syncJsonData = function(obj) {
	var topic_array = [];
	var qos_array = [];

	for(var topic in obj.subscribes) {
	    var qos = obj.subscribes[topic];
	    qos_array.push(qos);
	    topic_array.push(topic);
	}

	$scope.topics = topic_array;
	$scope.qoss = qos_array;
    }

    $scope.get_qos = function(index) {
	return $scope.qoss[index];
    }

    $scope.addSubscriber = function() {
	$scope.topics.push("");
	$scope.qoss.push(0);
    }

});

// GPIO config
configControllers.controller('GpioCtrl', function($scope, $resource) {
    $scope.pin_list = []; for(var i=1; i<100; i++){ $scope.pin_list.push(i); }
    $scope.mode_list = ["in", "out"];
    $scope.edge_list = ["rising", "falling", "both", "none"];
    $scope.pull_list = ["up", "down", "none"];
    $scope.gpio = undefined;

    var Gpio = $resource('/api/config/gpio.json', {});
    var gpio = Gpio.query(function() {
	$scope.gpio = gpio;
    });

    $scope.save = function() {
	console.log("save");
	gpio.forEach(function(pin) { pin.$save(); });
    }
});
