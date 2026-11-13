from pathlib import Path
import json

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    confusion_matrix,
    classification_report,
    roc_auc_score
)

# Load the saved training and test datasets
DATA_DIR = Path("data")

with open(DATA_DIR / "train_core_vs_neg.json", "r", encoding="utf-8") as f:
    train_data = json.load(f)

with open(DATA_DIR / "test_core_vs_neg.json", "r", encoding="utf-8") as f:
    test_data = json.load(f)

# Separate texts and labels
X_train_texts = [t for (t, y) in train_data]
y_train = [y for (t, y) in train_data]

X_test_texts = [t for (t, y) in test_data]
y_test = [y for (t, y) in test_data]

print("Train size:", len(X_train_texts))
print("Test size:", len(X_test_texts))

# Convert text into TF-IDF features
vectorizer = TfidfVectorizer(
    lowercase=True,
    min_df=5,
    max_df=0.9
)

X_train = vectorizer.fit_transform(X_train_texts)
X_test = vectorizer.transform(X_test_texts)

print("\nTF-IDF matrix shapes:")
print("  Train:", X_train.shape)
print("  Test:", X_test.shape)

# Train logistic regression with default L2 regularization
clf = LogisticRegression(
    max_iter=1000
)

clf.fit(X_train, y_train)

# Predict labels and CORE probabilities
y_pred = clf.predict(X_test)
y_prob = clf.predict_proba(X_test)[:, 1]

# Evaluate predictions
print("\nConfusion matrix:")
print(confusion_matrix(y_test, y_pred))

print("\nClassification report:")
print(classification_report(y_test, y_pred, digits=4))

auc = roc_auc_score(y_test, y_prob)
print("ROC AUC:", round(auc, 4))

# Count non-zero coefficients
coefficients = clf.coef_[0]

print("\nTotal coefficients:", len(coefficients))
print("Non-zero coefficients:", sum(coefficients != 0))

# Sort words by coefficient
words = vectorizer.get_feature_names_out()
sorted_indices = coefficients.argsort()

print("\nTop 15 positive-weight words (CORE):")
for i in sorted_indices[::-1][:15]:
    if coefficients[i] > 0:
        print(words[i], round(coefficients[i], 4))

print("\nTop 15 negative-weight words (NEG):")
for i in sorted_indices[:15]:
    if coefficients[i] < 0:
        print(words[i], round(coefficients[i], 4))

# Find false positives and false negatives
false_positives = []
false_negatives = []

for i in range(len(y_test)):
    if y_test[i] == 0 and y_pred[i] == 1:
        false_positives.append(i)
    elif y_test[i] == 1 and y_pred[i] == 0:
        false_negatives.append(i)

# Select five of each error type
review_indices = false_positives[:5] + false_negatives[:5]

# Fill any remaining places if one error type has fewer than five
for i in false_positives + false_negatives:
    if len(review_indices) >= 10:
        break
    if i not in review_indices:
        review_indices.append(i)

# Print full passages for manual review
print("\nMisclassified chunks for manual review:")

for number, i in enumerate(review_indices, start=1):
    print("\nExample:", number)
    print("Test position:", i)

    if y_test[i] == 0:
        print("Error type: False positive")
    else:
        print("Error type: False negative")

    print("Keyword label:", y_test[i])
    print("Model prediction:", y_pred[i])
    print("CORE probability:", round(y_prob[i], 4))
    print("Text:")
    print(X_test_texts[i])