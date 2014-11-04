module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        clean: ["gen"],
        dotPsci: {
            src: ["src/purs/*.purs", "bower_components/**/src/**/*.purs"]
        },         
        psc: {
            options : {
            },
            build : {
                src: ["src/purs/*.purs", "bower_components/**/src/**/*.purs"],
                dest: "gen/Snake.js"
            }
        },
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["psc:build"]);
};
