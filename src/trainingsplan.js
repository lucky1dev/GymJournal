import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

import { initializeApp } from 'firebase/app';
import { getAuth, signInWithPopup, signOut, GoogleAuthProvider, onAuthStateChanged } from "firebase/auth";
import { query, getFirestore, collection, addDoc, onSnapshot } from "firebase/firestore"; 

const firebaseConfig = {
  // Firebase-Konfigurationsdaten hier einfügen
};

const firebaseApp = initializeApp(firebaseConfig);

const provider = new GoogleAuthProvider();
const auth = getAuth();
const db = getFirestore();

const app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.signIn.subscribe(() => {
  console.log("LogIn called");
  signInWithPopup(auth, provider)
    .then(result => {
      result.user.getIdToken().then(idToken => {
        app.ports.signInInfo.send({
          token: idToken,
          email: result.user.email,
          uid: result.user.uid
        });
      });
    })
    .catch(error => {
      app.ports.signInError.send({
        code: error.code,
        message: error.message
      });
    });
});

app.ports.signOut.subscribe(() => {
  console.log("LogOut called");
  signOut(auth);
});

onAuthStateChanged(auth, user => {
  console.log("called");
  if (user) {
    user
      .getIdToken()
      .then(idToken => {
        app.ports.signInInfo.send({
          token: idToken,
          email: user.email,
          uid: user.uid
        });
      })
      .catch(error => {
        console.log("Error when retrieving cached user");
        console.log(error);
      });

    const q = query(collection(db, `users/${user.uid}/workoutplans`));
    onSnapshot(q, querySnapshot => {
      console.log("Received new snapshot");
      const workoutPlans = [];

      querySnapshot.forEach(doc => {
        const data = doc.data();
        const exercises = data.exercises.map(exercise => {
          return {
            name: exercise.name,
            sets: exercise.sets,
            reps: exercise.reps
          };
        });

        workoutPlans.push({
          id: data.id,
          title: data.title,
          weekday: data.weekday,
          exercises: exercises
        });
      });

      app.ports.receiveWorkoutPlans.send({
        workoutPlans: workoutPlans
      });
    });
  }
});

app.ports.saveWorkoutPlan.subscribe(data => {
  console.log(`Saving workout plan to database: ${data.title}`);

  const exercises = data.exercises.map(exercise => {
    return {
      name: exercise.name,
      sets: exercise.sets,
      reps: exercise.reps
    };
  });

  addDoc(collection(db, `users/${data.uid}/workoutplans`), {
    id: data.id,
    title: data.title,
    weekday: data.weekday,
    exercises: exercises
  }).catch(error => {
    app.ports.signInError.send({
      code: error.code,
      message: error.message
    });
  });
});

serviceWorker.unregister();
