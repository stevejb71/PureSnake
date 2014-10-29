module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        clean: ["gen"],
        psc: {
            options : {
            },
            build : {
                src: ["src/purs/Snake.purs"],
                dest: "gen/Snake.js"
            }
        },
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["psc:build"]);
};
