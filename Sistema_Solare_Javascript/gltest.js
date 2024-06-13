import * as THREE from 'https://threejs.org/build/three.module.js';
import { OrbitControls } from 'https://threejs.org/examples/jsm/controls/OrbitControls.js';

$(document).ready(function(){  
    //Scena
    var scene = new THREE.Scene();

    
    //Camera
    var camera = new THREE.PerspectiveCamera(65, window.innerWidth/window.innerHeight, 0.1, 5000);
    //Prospettiva della camera
    /*
    //Posizione
    camera.position.x = 0;
    camera.position.y = 400; 
    camera.position.z = 800;
    //Rotazione
    camera.rotation.x = -0.6;
    camera.rotation.y = 0;
    camera.rotation.z = 0;
   */
    //Posizione
    camera.position.x = 0;
    camera.position.y = 250; 
    camera.position.z = 1100;
    //Rotazione
    camera.rotation.x = -0.75;
    camera.rotation.y = 0;
    camera.rotation.z = 0;

    
    //WebGl renderer
    var renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);


    //Orbit controls
    var controls = new OrbitControls( camera, renderer.domElement );
    controls.minDistance = 200;
    controls.maxDistance = 5000;
    

    //Solar system
    //Variabili
    var geometry, material;
    
    //Dimensione
    var sunDim = 100;
    var earthDim = 15;
    var moonDim = earthDim/3;
    var marsDim = earthDim - (earthDim*20/100);
    var jupiterDim = earthDim*3;
    var ganymedeDim = earthDim/3;
    var saturnDim = jupiterDim - (jupiterDim*10/100);
    var mercuryDim = earthDim/2;
    var venusDim = earthDim;
    var uranusDim = earthDim*2;
    var neptuneDim = earthDim*2;
    //Distanza dal sole o dai pianeti
    var earthD = sunDim * 2.5;
    var moonD = earthDim + (earthDim*3/100);
    var marsD = earthD + (earthD*30/100);
    var jupiterD = earthD * 1.7;
    var ganymedeD = jupiterDim + (jupiterDim*30/100);
    var saturnD = earthD * 2.3;
    var mercuryD =  earthD - (earthD*50/100);
    var venusD =  earthD - (earthD*30/100);
    var uranusD = earthD * 2.75;
    var neptuneD = earthD * 3.1;
    //Velocità di rotazione
    var earthV = 0.001;
    var moonV = earthV * 5;
    var marsV = earthV / 1.5;
    var jupiterV = earthV / 2;
    var ganymedeV = jupiterV * 7;
    var saturnV = earthV / 2.5;
    var mercuryV = earthV * 3;
    var venusV = earthV * 2;
    var uranusV = earthV / 3;
    var neptuneV = earthV / 4;


    //Sorgente di illuminazione (sole)
    var light = new THREE.PointLight(0xffffff, 2.0);
    //light.position.set(-1, 1, 2).normalize();
    light.position.set(0, 0, 0).normalize();
    light.castShadow = true;
    scene.add(light);

    /*//Luci
    //X
    var light1 = new THREE.DirectionalLight(0xffffff, 1.5);
    light1.position.set(1, 0, 0).normalize();
    scene.add(light1);   

    var light2 = new THREE.DirectionalLight(0xffffff, 1.5);
    light2.position.set(-1, 0, 0).normalize();
    scene.add(light2);

    //Y
    var light3 = new THREE.DirectionalLight(0xffffff, 1.5);
    light3.position.set(0, 1, 0).normalize();
    scene.add(light3);

    var light4 = new THREE.DirectionalLight(0xffffff, 1.5);
    light4.position.set(0, -1, 0).normalize();
    scene.add(light4);

    //Z
    var light5 = new THREE.DirectionalLight(0xffffff, 1.5);
    light5.position.set(0, 0, -1).normalize();
    scene.add(light5);

    var light6 = new THREE.DirectionalLight(0xffffff, 1.5);
    light6.position.set(0, 0, 1).normalize();
    scene.add(light6);
    */

    //Sun
    geometry = new THREE.SphereGeometry( sunDim, 30, 30 ); //raggio, lunghezza segmenti, altezza segmenti
    material = new THREE.MeshBasicMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/sun.jpg')} );
    var sun = new THREE.Mesh(geometry, material);
    sun.matrixAutoUpdate = false;
    sun.castShadow = false;
    scene.add(sun);


    //Mercury
    geometry = new THREE.SphereGeometry( mercuryDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/mercury.jpg')} );
    var mercury = new THREE.Mesh(geometry, material);
    mercury.matrixAutoUpdate = false;
    sun.add(mercury);


    //Venus
    geometry = new THREE.SphereGeometry( venusDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/venus.jpg')} );
    var venus = new THREE.Mesh(geometry, material);
    venus.matrixAutoUpdate = false;
    sun.add(venus);
    

    //Earth
    geometry = new THREE.SphereGeometry( earthDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/earth.jpg')} );
    var earth = new THREE.Mesh(geometry, material);
    earth.matrixAutoUpdate = false;
    sun.add(earth);
    

    //Moon
    geometry = new THREE.SphereGeometry( moonDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/moon.jpg')} );
    var moon = new THREE.Mesh(geometry, material);
    moon.matrixAutoUpdate = false;
    earth.add(moon);
    

    //Mars
    geometry = new THREE.SphereGeometry( marsDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/mars.jpg')} );
    var mars = new THREE.Mesh(geometry, material);
    mars.matrixAutoUpdate = false;
    sun.add(mars);
    

    //Jupiter
    geometry = new THREE.SphereGeometry( jupiterDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/jupiter.jpg')} );
    var jupiter = new THREE.Mesh(geometry, material);
    jupiter.matrixAutoUpdate = false;
    sun.add(jupiter);
    

    //Ganymede
    geometry = new THREE.SphereGeometry( ganymedeDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/ganymede.jpg')} );
    var ganymede = new THREE.Mesh(geometry, material);
    ganymede.matrixAutoUpdate = false;
    jupiter.add(ganymede);


    //Saturn
    geometry = new THREE.SphereGeometry( saturnDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/saturn.jpg')} );
    var saturn = new THREE.Mesh(geometry, material);
    saturn.matrixAutoUpdate = false;
    sun.add(saturn);

    //Saturn ring 1
    var satr_inner, satr_outer;
    //1
    satr_inner = saturnDim + (saturnDim*15/100);
    satr_outer = satr_inner + (satr_inner*10/100);
    geometry = new THREE.RingGeometry(satr_inner, satr_outer, 30 ); //innerRadius, outerRadius
    material = new THREE.MeshBasicMaterial( { color: 0x32302F, side: THREE.DoubleSide} ); //ccccb3
    var satr1 = new THREE.Mesh( geometry, material );
    satr1.rotation.x = Math.PI/4;
    saturn.add(satr1);
    //2
    satr_inner = saturnDim + (saturnDim*26/100);
    satr_outer = satr_inner + (satr_inner*10/100);
    geometry = new THREE.RingGeometry(satr_inner, satr_outer, 30 ); 
    material = new THREE.MeshBasicMaterial( { color: 0x4B4846, side: THREE.DoubleSide} );
    var satr2 = new THREE.Mesh( geometry, material );
    satr2.rotation.x =  Math.PI/4;
    saturn.add(satr2);
    //3
    satr_inner = saturnDim + (saturnDim*35/100);
    satr_outer = satr_inner + (satr_inner*10/100);
    geometry = new THREE.RingGeometry(satr_inner, satr_outer, 30 ); 
    material = new THREE.MeshBasicMaterial( { color: 0x948C8A, side: THREE.DoubleSide} );
    var satr3 = new THREE.Mesh( geometry, material );
    satr3.rotation.x =  Math.PI/4;
    saturn.add(satr3);
    //4
    satr_inner = saturnDim + (saturnDim*55/100);
    satr_outer = satr_inner + (satr_inner*10/100);
    geometry = new THREE.RingGeometry(satr_inner, satr_outer, 30 ); 
    material = new THREE.MeshBasicMaterial( { color: 0x32302F, side: THREE.DoubleSide} );
    var satr4 = new THREE.Mesh( geometry, material );
    satr4.rotation.x =  Math.PI/4;
    saturn.add(satr4);
    

    //Uranus
    geometry = new THREE.SphereGeometry( uranusDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/uranus.jpg')} );
    var uranus = new THREE.Mesh(geometry, material);
    uranus.matrixAutoUpdate = false;
    sun.add(uranus);
    

    //Neptune
    geometry = new THREE.SphereGeometry( neptuneDim, 30, 30 );
    material = new THREE.MeshLambertMaterial( {map:THREE.ImageUtils.loadTexture('solar_system_texture/neptune.jpg')} );
    var neptune = new THREE.Mesh(geometry, material);
    neptune.matrixAutoUpdate = false;
    sun.add(neptune);



    //Orbite
    var orb_inner, orb_outer;
    material = new THREE.MeshBasicMaterial( { color: 0xccccb3, side: THREE.DoubleSide} ); //ccccb3

    orb_inner = mercuryD-0.1;
    orb_outer = mercuryD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var mercuryO = new THREE.Mesh( geometry, material );
    mercuryO.rotation.x = Math.PI/2;
    sun.add(mercuryO);

    orb_inner = venusD-0.1;
    orb_outer = venusD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var venusO = new THREE.Mesh( geometry, material );
    venusO.rotation.x = Math.PI/2;
    sun.add(venusO);

    orb_inner = earthD-0.1;
    orb_outer = earthD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var earthO = new THREE.Mesh( geometry, material );
    earthO.rotation.x = Math.PI/2;
    sun.add(earthO);

    orb_inner = marsD-0.1;
    orb_outer = marsD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var marsO = new THREE.Mesh( geometry, material );
    marsO.rotation.x = Math.PI/2;
    sun.add(marsO);

    orb_inner = jupiterD-0.1;
    orb_outer = jupiterD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var jupiterO = new THREE.Mesh( geometry, material );
    jupiterO.rotation.x = Math.PI/2;
    sun.add(jupiterO);

    orb_inner = saturnD-0.1;
    orb_outer = saturnD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var saturnO = new THREE.Mesh( geometry, material );
    saturnO.rotation.x = Math.PI/2;
    sun.add(saturnO);

    orb_inner = uranusD-0.1;
    orb_outer = uranusD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var uranusO = new THREE.Mesh( geometry, material );
    uranusO.rotation.x = Math.PI/2;
    sun.add(uranusO);

    orb_inner = neptuneD-0.1;
    orb_outer = neptuneD+0.1;
    geometry = new THREE.RingGeometry(orb_inner, orb_outer, 100 ); //innerRadius, outerRadius
    var neptuneO = new THREE.Mesh( geometry, material );
    neptuneO.rotation.x = Math.PI/2;
    sun.add(neptuneO);


    //Update controls
    controls.update();


    //Animazioni
    var render_scene = function(){

        var now = Date.now();
        var dt = now - (render_scene.time||now);
        render_scene.time = now;

        //Rotazione terra
        var earthTS = new THREE.Matrix4().makeTranslation(earthD, 0, 0); //Traslazione della terra rispetto al sole
        var earthRS = new THREE.Matrix4().makeRotationY(earthV*render_scene.time);
        //Rotazione rispetto al sole
        earth.matrix = earthRS.multiply(earthTS); 
        
        //Rotazione luna
        var moonT = new THREE.Matrix4().makeTranslation(moonD, 0, 0);
        var moonR = new THREE.Matrix4().makeRotationY(moonV*render_scene.time);
        //Rotazione rispetto alla terra
        moon.matrix = moonR.multiply(moonT);

        //Rotazione marte
        var marsT = new THREE.Matrix4().makeTranslation(marsD, 0, 0);
        var marsR = new THREE.Matrix4().makeRotationY(marsV*render_scene.time);
        //Rotazione rispetto al sole
        mars.matrix = marsR.multiply(marsT);

        //Rotazione giove
        var jupiterT = new THREE.Matrix4().makeTranslation(jupiterD, 0, 0);
        var jupiterR = new THREE.Matrix4().makeRotationY(jupiterV*render_scene.time);
        //Rotazione rispetto al sole
        jupiter.matrix = jupiterR.multiply(jupiterT);

        //Rotazione ganymede
        var ganymedeT = new THREE.Matrix4().makeTranslation(ganymedeD, 0, 0);
        var ganymedeR = new THREE.Matrix4().makeRotationY(ganymedeV*render_scene.time);
        //Rotazione rispetto a giove
        ganymede.matrix = ganymedeR.multiply(ganymedeT);

        //Rotazione saturno
        var saturnT = new THREE.Matrix4().makeTranslation(saturnD, 0, 0);
        var saturnR = new THREE.Matrix4().makeRotationY(saturnV*render_scene.time);
        //Rotazione rispetto al sole
        saturn.matrix = saturnR.multiply(saturnT);

        //Rotazione mercurio
        var mercuryT = new THREE.Matrix4().makeTranslation(mercuryD, 0, 0);
        var mercuryR = new THREE.Matrix4().makeRotationY(mercuryV*render_scene.time);
        //Rotazione rispetto al sole
        mercury.matrix = mercuryR.multiply(mercuryT);

        //Rotazione venere
        var venusT = new THREE.Matrix4().makeTranslation(venusD, 0, 0);
        var venusR = new THREE.Matrix4().makeRotationY(venusV*render_scene.time);
        //Rotazione rispetto al sole
        venus.matrix = venusR.multiply(venusT);

        //Rotazione uranio
        var uranusT = new THREE.Matrix4().makeTranslation(uranusD, 0, 0);
        var uranusR = new THREE.Matrix4().makeRotationY(uranusV*render_scene.time);
        //Rotazione rispetto al sole
        uranus.matrix = uranusR.multiply(uranusT);

        //Rotazione nettuno
        var neptuneT = new THREE.Matrix4().makeTranslation(neptuneD, 0, 0);
        var neptuneR = new THREE.Matrix4().makeRotationY(neptuneV*render_scene.time);
        //Rotazione rispetto al sole
        neptune.matrix = neptuneR.multiply(neptuneT);

        renderer.render(scene, camera);
        requestAnimationFrame(render_scene);

        //Update controls
        controls.update();
        //console.log("Rendering");
    }
    
    render_scene();
});