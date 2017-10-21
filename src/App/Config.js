exports.config = {
  title: "EAK",
  public_path: process.env.NODE_ENV === 'production'
               ? '/dist/'
               : 'http://localhost:8080/dist/'
}
