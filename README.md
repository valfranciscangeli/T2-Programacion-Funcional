# Tarea 2 - Programación Funcional

**Autor**: Valeria Franciscangeli

## Descripción

Este proyecto corresponde a la Tarea 2 de Programación Funcional, implementada en Haskell. Para facilitar la ejecución y la gestión de dependencias, el proyecto está configurado como un proyecto de Cabal.

## Estructura del Proyecto
Para mantener el orden se dividió el desarrollo en 2 archivos:
- **Código fuente**: El código de la tarea se encuentra en el archivo `app/T2.hs`.
- **Pruebas**: La función `main` en el archivo `app/Main.hs` ejecuta tests basados en los ejemplos proporcionados en el enunciado, así como otros tests adicionales para verificar el correcto funcionamiento de las funciones implementadas.

## Ejecución

Para ejecutar el proyecto, se utiliza el comando:

```bash
cabal run
```

Este comando compila y ejecuta el proyecto, corriendo los tests definidos en main.

## Notas

Se agregaron especificaciones de tipo en ciertos lugares del código para evitar warnings relacionados con la inferencia de tipos por parte del compilador.