Vue.use(VueCharts);

var foosman2App = new Vue({
    el: '#foosman2App',
    data: {
    	newPlayerName: "",
        newGameSingle: {
            winner: "",
            looser: ""
        },
        newGameDouble: {
            winner1: "",
            winner2: "",
            looser1: "",
            looser2: ""
        },
    	players: [],
        playerDetails: null,
        chartoption:{
            responsive:true,
            maintainAspectRatio:true,
            title: {
                    display: false
            },
            scales: {
                yAxes: [{
                    stacked: true
                }]
            }
        },
        playerPointsV1History: [],
        playerPointsV1Label: "Points V1"
    },
    methods: {
    	refreshData: function () {
    		var that = this;
    		smackjack.allPlayers(function (res) {
                that.players = res.sort(function (a, b) {
                    return b.pointsV1 - a.pointsV1;
                });
    		});
    	},
        displayPlayer: function (name) {
            var that = this;
            smackjack.getPlayerDetails(name, function (res) {
                that.playerDetails = res;
                that.playerPointsV1History = res.pointsV1History.reverse();
            });
        },
        closeDetails: function () {
            this.playerDetails = null;
        },
    	initiateNewPlayer: function () {
    		$("#newPlayerForm").modal("show");
    	},
        initiateNewSingleGame: function () {
            $("#newGameSingleForm").modal("show");
        },
        initiateNewDoubleGame: function () {
            $("#newGameDoubleForm").modal("show");
        },
    	saveNewPlayer: function () {
    		var that = this;
    		$("#newPlayerForm").modal("hide");
    		var res = smackjack.newPlayer(this.newPlayerName, function (res) {
    			that.newPlayerName = "";
    			that.refreshData();
    		});
    	},
        saveNewGameSingle: function () {
            var that = this;
            $("#newGameSingleForm").modal("hide");
            var res = smackjack.newGameSingle(this.newGameSingle, function (res) {
                that.newGameSingle.winner = "";
                that.newGameSingle.looser = "";
                that.refreshData();
            });  
        },
        saveNewGameDouble: function () {
            var that = this;
            $("#newGameDoubleForm").modal("hide");
            var res = smackjack.newGameDouble(this.newGameDouble, function (res) {
                that.newGameDouble.winner1 = "";
                that.newGameDouble.winner2 = "";
                that.newGameDouble.looser1 = "";
                that.newGameDouble.looser2 = "";
                that.refreshData();
            });  
        }
    },
    mounted: function () {
        this.refreshData();
    },
});
