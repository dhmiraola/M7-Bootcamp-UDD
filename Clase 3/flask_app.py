from flask import Flask, request, jsonify
import pandas as pd
import joblib

app = Flask(__name__)

# Cargar modelo una sola vez (BUENA PRÁCTICA)
model = joblib.load("classifier.pkl")

@app.route("/predice", methods=["POST"])
def predict():
    data = request.get_json()

    if data is None:
        return jsonify({"error": "JSON inválido"}), 400

    df = pd.DataFrame([data])
    df = pd.get_dummies(df)

    # IMPORTANTE: alinear columnas con el modelo
    model_features = model.feature_names_in_
    df = df.reindex(columns=model_features, fill_value=0)

    prediction = model.predict(df)[0]

    return jsonify({
        "survived": bool(prediction),
        "message": "Sobrevive" if prediction == 1 else "No sobrevive"
    })

if __name__ == "__main__":
    app.run(port=8000, debug=True)