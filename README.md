# purescript-colorpicker-halogen

A bunch of components that could be used to build colorpicker.

## Idea

Instead of providing ready one large atomic colorpicker component this library defines small
reusable components that takes a `Color` as input and raises messages containing `Color`.

Having external (e.g. in parent component state) `Color` would be enough to bake it all into
one colorpicker.

## Examples

To run examples

```bash
npm run build
http-server example
```

you can install [http-server using npm](https://www.npmjs.com/package/http-server)
