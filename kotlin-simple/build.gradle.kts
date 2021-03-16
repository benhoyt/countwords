import org.gradle.jvm.tasks.Jar

plugins {
    kotlin("jvm") version "1.4.31"
    application
}

dependencies {
    kotlin("stdlib")
    implementation(kotlin("stdlib-jdk8"))
}

application {
    mainClass.set("KotlinSimpleKt")
}

repositories {
    mavenCentral()
}

val fatJar = task("fatJar", type = Jar::class) {
    baseName = "${project.name}-fat"
    manifest {
        attributes["Main-Class"] = "KotlinSimpleKt"
    }
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    with(tasks.jar.get() as CopySpec)
}

tasks {
    "build" {
        dependsOn(fatJar)
    }
}
