function explainPipe(pipe) {
    return db.point_data.explain().aggregate(pipe).queryPlanner.winningPlan.slotBasedPlan.stages;
}


// Modified to include the whole dataset...
const openLowHighClose = [
  {"$match":{"$expr":{"$gte":[
    "$time",{"$dateSubtract":{"startDate": new Date("2022-01-08T00:00:00.000Z"),"unit":"hour","amount":500000}}
  ]}}},
  {"$sort":{"time":1}},
  {"$group":{
    "_id":{"symbol":"$tags.symbol","time":{"$dateTrunc":{"date":"$time","unit":"minute","binSize":240}}}, // T2 = 4h
	"high":{"$max":"$price"},
	"low":{"$min":"$price"},
	"open":{"$first":"$price"},
	"close":{"$last":"$price"}
  }},
];

const andOfSelective = [
    {
        $match:
        {
            $and:
            [
                { "array1": 10 },
                { "array2": { $gt: 99.9 } },
                { "array3": { $gt: 999.9 } },
            ],
        },
    },
    { $count: "count" },
];
