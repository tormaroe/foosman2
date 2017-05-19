// foosman2.js

var foosman2App = new Vue({
    el: '#foosman2App',
    data: {
    	newPlayerName: "",
    	players: []
    },
    methods: {
    	refreshData: function () {
    		var that = this;
    		smackjack.allPlayers(function (res) {
    			that.players = res;
    		});
    	},
    	initiateNewPlayer: function () {
    		$("#newPlayerForm").modal("show");
    	},
    	saveNewPlayer: function () {
    		var that = this;
    		$("#newPlayerForm").modal("hide");
    		var res = smackjack.newPlayer(this.newPlayerName, function (res) {
    			that.newPlayerName = "";
    			that.refreshData();
    		});
    	}
    },
    mounted: function () {
        this.refreshData();
    },
});
