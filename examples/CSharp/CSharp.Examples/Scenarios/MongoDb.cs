﻿using System;
using System.Linq;

using MongoDB.Bson;
using MongoDB.Bson.Serialization.Attributes;
using MongoDB.Driver;

using NBomber.Contracts;
using NBomber.CSharp;

namespace CSharp.Examples.Scenarios
{
    class MongoDbScenario
    {
        public class User
        {
            [BsonId]
            public ObjectId Id { get; set; }
            public string Name { get; set; }
            public int Age { get; set; }
            public bool IsActive { get; set; }
        }

        public static Scenario BuildScenario()
        {
            var db = new MongoClient().GetDatabase("Test");

            Action initDb = () =>
            {   
                var testData = Enumerable.Range(0, 2000)
                    .Select(i => new User { Name = $"Test User {i}", Age = i, IsActive = true })
                    .ToList();

                db.DropCollection("Users");
                db.GetCollection<User>("Users").InsertMany(testData);
            };

            var usersCollection = db.GetCollection<User>("Users");

            var step1 = Step.CreatePull("read IsActive = true and TOP 500", ConnectionPool.None, async context =>
            {
                await usersCollection.Find(u => u.IsActive == true)
                                     .Limit(500)
                                     .ToListAsync();
                return Response.Ok();
            });

            var step2 = Step.CreatePull("read Age > 50 and TOP 100", ConnectionPool.None, async context =>
            {
                await usersCollection.Find(u => u.IsActive == true)
                                     .Limit(500)
                                     .ToListAsync();
                return Response.Ok();
            });

            return ScenarioBuilder.CreateScenario("test_mongo", step1, step2)
                                  .WithTestInit(initDb);                                  
        }
    }
}
